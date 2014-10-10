#include "storage.h"
#include "hash.h"

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <sys/mman.h>
#include <string.h>
#include <unistd.h>
#include <stdio.h>

unsigned int block_length_get(struct block *blk)
{
	unsigned int shift1 = LEN_SIZE % 8;
	unsigned int mask1 = (1 << shift1) - 1;
	unsigned int shift2 = 8 - shift1;
	unsigned int length;

	length = blk->lenptr[0] << shift1;
	length |= ((blk->lenptr[1] >> shift2) & mask1);

	return length;
}

void block_length_set(struct block *blk, unsigned int length)
{
	unsigned int shift1 = LEN_SIZE % 8;
	unsigned int mask1 = (1 << shift1) - 1;
	unsigned int shift2 = 8 - shift1;
	unsigned int mask2 = (1 << shift2) - 1;

	blk->lenptr[0] = (length >> shift1) & 0xff;
	blk->lenptr[1] &= mask2;
	blk->lenptr[1] |= (length & mask1) << shift2;
}

unsigned long block_ptr_get(struct block *blk)
{
	unsigned int shift = 8 - (LEN_SIZE % 8);
	unsigned int mask = (1 << shift) - 1;
	unsigned long ptr;
	int i;

	ptr = blk->lenptr[1] & mask;

	for (i = 2; i < 8; i++) {
		ptr <<= 8;
		ptr |= blk->lenptr[i];
	}

	return ptr;
}

void block_ptr_set(struct block *blk, unsigned long ptr)
{
	unsigned int shift1 = LEN_SIZE % 8;
	unsigned int mask1 = (1 << shift1) - 1;
	unsigned int shift2 = 8 - shift1;
	unsigned int mask2 = (1 << shift2) - 1;
	int i;

	blk->lenptr[1] &= mask1 << shift2;
	blk->lenptr[1] |= (ptr >> 48) & mask2;

	for (i = 7; i >= 2; i--) {
		blk->lenptr[i] = ptr & 0xff;
		ptr >>= 8;
	}
}

unsigned long block_extra_ptr_get(struct block *blk)
{
	unsigned long ptr = 0;
	int i;

	for (i = 0; i < ADDR_SIZE_BYTES; i++) {
		ptr <<= 8;
		ptr |= blk->data[i];
	}
	return ptr;
}

void block_extra_ptr_set(struct block *blk, unsigned long ptr)
{

	int i;

	for (i = ADDR_SIZE_BYTES - 1; i >= 0; i--) {
		blk->data[i] = ptr & 0xff;
		ptr >>= 8;
	}
}

static inline int block_check_status(struct block *blk, int shift)
{
	return (blk->status & (1 << shift)) != 0;
}

static inline void block_set_status(struct block *blk, int shift)
{
	blk->status |= (1 << shift);
}

static inline void block_unset_status(struct block *blk, int shift)
{
	blk->status &= ~(1 << shift);
}

static inline int block_is_key(struct block *blk)
{
	return block_check_status(blk, STATUS_KEY);
}

static inline void block_set_key(struct block *blk)
{
	return block_set_status(blk, STATUS_KEY);
}

static inline void block_unset_key(struct block *blk)
{
	return block_unset_status(blk, STATUS_KEY);
}

static inline int block_is_occupied(struct block *blk)
{
	return block_check_status(blk, STATUS_OCCUPIED);
}

static inline void block_set_occupied(struct block *blk)
{
	return block_set_status(blk, STATUS_OCCUPIED);
}

static inline void block_unset_occupied(struct block *blk)
{
	return block_unset_status(blk, STATUS_OCCUPIED);
}

static inline int block_is_last(struct block *blk)
{
	return block_check_status(blk, STATUS_LAST);
}

static inline void block_set_last(struct block *blk)
{
	return block_set_status(blk, STATUS_LAST);
}

static inline void block_unset_last(struct block *blk)
{
	return block_unset_status(blk, STATUS_LAST);
}

int block_data_get(struct block *blk, void *dest)
{
	int len = block_length_get(blk);
	void *data_start = blk->data;

	if (!block_is_occupied(blk))
		return -1;

	if (block_is_key(blk))
		data_start += ADDR_SIZE_BYTES;

	memcpy(dest, data_start, len);
	return len;
}

int block_data_set(struct block *blk, void *src, size_t len)
{
	int available = sizeof(blk->data);
	void *data_start = blk->data;

	if (block_is_key(blk)) {
		available -= ADDR_SIZE_BYTES;
		data_start += ADDR_SIZE_BYTES;
	}

	if (len > available)
		return -1;

	block_length_set(blk, len);
	block_set_occupied(blk);
	memcpy(data_start, src, len);
	return 0;
}

int store_init(struct store *store)
{
	void *ptr;
	int i;

	store->fd = open("/dev/zero", O_RDWR);
	if (store->fd < 0)
		return -1;

	ptr = mmap(0, BLOCK_SIZE * NUM_BLOCKS,
		PROT_READ|PROT_WRITE, MAP_PRIVATE, store->fd, 0);
	if (ptr == MAP_FAILED) {
		close(store->fd);
		return -1;
	}
	store->blocks = (struct block *) ptr;

	for (i = 0; i < NUM_ENTRIES; i++) {
		store->entries[i] = NO_ADDR;
	}

	return 0;
}

void store_cleanup(struct store *store)
{
	munmap((void *) store->blocks, BLOCK_SIZE * NUM_BLOCKS);
	close(store->fd);
}

static int find_empty_block(struct store *store, int start)
{
	int i;

	for (i = start; i < NUM_BLOCKS; i++) {
		if (!block_is_occupied(&store->blocks[i]))
			return i;
	}

	return -1;
}

static inline int floor_div(int a, int b)
{
	return ((a - 1) / b) + 1;
}

static int store_get_index(struct store *store,
		unsigned char *key, size_t key_len)
{
	uint64_t hash;
	uint64_t keypos;
	unsigned char actual_key[MAX_KEY_SIZE];
	unsigned int actual_len;
	struct block *blk;

	hash = HASH_FUNC(key, key_len) % NUM_ENTRIES;

	if (store->entries[hash] == NO_ADDR)
		return -1;

	keypos = store->entries[hash];

	for (;;) {
		if (keypos >= NUM_BLOCKS)
			break;
		blk = &store->blocks[keypos];
		if (!block_is_key(blk) || !block_is_occupied(blk))
			return -1;
		actual_len = block_length_get(blk);
		block_data_get(blk, actual_key);

		if (actual_len == key_len &&
				memcmp(key, actual_key, key_len) == 0)
			return keypos;

		if (block_is_last(blk))
			break;

		keypos = block_extra_ptr_get(blk);
	}

	return -1;
}

static int store_put_empty(struct store *store,
		unsigned char *key, size_t key_len,
		unsigned char *value, size_t value_len,
		uint64_t hash, int req_blocks)
{
	int i;
	int lastpos, keypos, datapos[req_blocks];
	struct block *keyblk, *valblk;
	unsigned char *data = value;

	lastpos = keypos = find_empty_block(store, 0);
	if (keypos < 0)
		return -1;

	for (i = 0; i < req_blocks; i++) {
		lastpos = datapos[i] = find_empty_block(store, lastpos + 1);
		if (lastpos < 0)
			return -1;
	}

	keyblk = &store->blocks[keypos];

	block_set_last(keyblk);
	block_set_key(keyblk);

	block_ptr_set(keyblk, datapos[0]);
	block_data_set(keyblk, key, key_len);

	for (i = 0; i < req_blocks - 1; i++) {
		valblk = &store->blocks[datapos[i]];
		block_unset_last(valblk);
		block_unset_key(valblk);

		block_ptr_set(valblk, datapos[i + 1]);
		block_data_set(valblk, data, BLOCK_DATA_SIZE);
		data += BLOCK_DATA_SIZE;
	}

	valblk = &store->blocks[datapos[req_blocks - 1]];
	block_set_last(valblk);
	block_unset_key(valblk);

	block_data_set(valblk, data, value_len % BLOCK_DATA_SIZE);

	store->entries[hash] = keypos;

	return 0;
}

int store_put(struct store *store,
		unsigned char *key, size_t key_len,
		unsigned char *value, size_t value_len)
{
	int req_blocks = floor_div(value_len, BLOCK_DATA_SIZE);
	uint64_t hash;

	if (key_len > MAX_KEY_SIZE || value_len > MAX_VALUE_SIZE)
		return -1;

	if (req_blocks + 1 > NUM_BLOCKS)
		return -1;

	hash = HASH_FUNC(key, key_len) % NUM_ENTRIES;

	if (store->entries[hash] == NO_ADDR)
		return store_put_empty(
				store, key, key_len,
				value, value_len,
				hash, req_blocks);

	return 0;
}

int store_get(struct store *store, unsigned char *key, size_t key_len,
		unsigned char* dest, size_t dest_len)
{
	int keypos, valpos;
	struct block *blk;
	unsigned int blk_len;
	unsigned int actual_len = 0;

	keypos = store_get_index(store, key, key_len);
	if (keypos < 0)
		return -1;
	blk = &store->blocks[keypos];
	valpos = block_ptr_get(blk);

	do {
		blk = &store->blocks[valpos];
		if (block_is_key(blk) || !block_is_occupied(blk))
			return -1;
		blk_len = block_length_get(blk);
		if (blk_len > dest_len)
			return -1;
		block_data_get(blk, dest);
		dest += blk_len;
		dest_len -= blk_len;
		actual_len += blk_len;
		valpos = block_ptr_get(blk);
	} while (!block_is_last(blk));

	return actual_len;
}
