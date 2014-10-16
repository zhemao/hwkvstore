#include "storage.h"
#include "hash.h"

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <sys/mman.h>
#include <string.h>
#include <unistd.h>
#include <stdio.h>

static unsigned int block_length_get(struct block *blk)
{
	unsigned int shift1 = LEN_SIZE % 8;
	unsigned int mask1 = (1 << shift1) - 1;
	unsigned int shift2 = 8 - shift1;
	unsigned int length;

	length = blk->lenptr[0] << shift1;
	length |= ((blk->lenptr[1] >> shift2) & mask1);

	return length;
}

static void block_length_set(struct block *blk, unsigned int length)
{
	unsigned int shift1 = LEN_SIZE % 8;
	unsigned int mask1 = (1 << shift1) - 1;
	unsigned int shift2 = 8 - shift1;
	unsigned int mask2 = (1 << shift2) - 1;

	blk->lenptr[0] = (length >> shift1) & 0xff;
	blk->lenptr[1] &= mask2;
	blk->lenptr[1] |= (length & mask1) << shift2;
}

static unsigned long block_ptr_get(struct block *blk)
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

static void block_ptr_set(struct block *blk, unsigned long ptr)
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

static int block_data_get(struct block *blk, void *dest)
{
	int len = block_length_get(blk);
	void *data_start = blk->data;

	if (!block_is_occupied(blk))
		return -1;

	memcpy(dest, data_start, len);
	return len;
}

static int block_data_set(struct block *blk, void *src, size_t len)
{
	int available = sizeof(blk->data);
	void *data_start = blk->data;

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

	if (pthread_rwlock_init(&store->rwlock, NULL))
		return -1;

	store->keys = calloc(NUM_ENTRIES, sizeof(struct key_entry));
	if (store->keys == NULL) {
		pthread_rwlock_destroy(&store->rwlock);
		return -1;
	}

	store->fd = open("/dev/zero", O_RDWR);
	if (store->fd < 0) {
		pthread_rwlock_destroy(&store->rwlock);
		return -1;
	}

	ptr = mmap(0, BLOCK_SIZE * NUM_BLOCKS,
		PROT_READ|PROT_WRITE, MAP_PRIVATE, store->fd, 0);
	if (ptr == MAP_FAILED) {
		close(store->fd);
		pthread_rwlock_destroy(&store->rwlock);
		return -1;
	}
	store->blocks = (struct block *) ptr;

	for (i = 0; i < NUM_ENTRIES; i++)
		store->keys[i].length = 0;

	return 0;
}

void store_cleanup(struct store *store)
{
	pthread_rwlock_destroy(&store->rwlock);
	munmap((void *) store->blocks, BLOCK_SIZE * NUM_BLOCKS);
	free(store->keys);
	close(store->fd);
}

static int64_t find_empty_block(struct store *store, int start)
{
	int64_t i;

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

static int allocate_blocks(struct store *store,
		uint64_t *datapos, size_t req_blocks)
{
	int64_t lastpos = 0;
	int i;

	for (i = 0; i < req_blocks; i++) {
		lastpos = find_empty_block(store, lastpos);
		if (lastpos < 0)
			return -1;
		datapos[i] = lastpos;
		lastpos++;
	}

	return 0;
}

static void fill_blocks(struct store *store,
		uint8_t *value, size_t value_len,
		uint64_t *datapos, size_t req_blocks)
{
	struct block *valblk;
	unsigned char *data = value;
	int i;

	for (i = 0; i < req_blocks - 1; i++) {
		valblk = &store->blocks[datapos[i]];
		block_unset_last(valblk);
		block_ptr_set(valblk, datapos[i + 1]);
		block_data_set(valblk, data, BLOCK_DATA_SIZE);
		data += BLOCK_DATA_SIZE;
	}

	valblk = &store->blocks[datapos[req_blocks - 1]];
	block_set_last(valblk);
	block_data_set(valblk, data, value_len % BLOCK_DATA_SIZE);
}

static void erase_blocks(struct store *store, uint64_t pos)
{
	struct block *blk;

	blk = &store->blocks[pos];

	while (!block_is_last(blk)) {
		block_unset_occupied(blk);
		pos = block_ptr_get(blk);
		blk = &store->blocks[pos];
	}

	block_unset_occupied(blk);
	block_unset_last(blk);
}

static void unlock_or_abort(pthread_rwlock_t *rwlock) {
	if (pthread_rwlock_unlock(rwlock)) {
		fprintf(stderr, "Unable to release rwlock\n");
		abort();
	}
}

static inline int key_empty(struct store *store, int hash)
{
	return store->keys[hash].length == 0;
}

static int key_match(struct store *store,
		uint8_t *key, size_t key_len, int hash)
{
	uint8_t *old_key, old_len;

	old_len = store->keys[hash].length;
	old_key = store->keys[hash].data;

	return old_len == key_len && memcmp(old_key, key, key_len) == 0;
}

static int put_hash(struct store *store, uint8_t *key, size_t key_len)
{
	int hash;

	hash = pearson_hash1(key, key_len) % NUM_ENTRIES;
	if (key_empty(store, hash) || key_match(store, key, key_len, hash))
		return hash;

	hash = pearson_hash2(key, key_len) % NUM_ENTRIES;
	if (key_empty(store, hash) || key_match(store, key, key_len, hash))
		return hash;

	return -1;
}

static int get_hash(struct store *store, uint8_t *key, size_t key_len)
{
	int hash;

	hash = pearson_hash1(key, key_len) % NUM_ENTRIES;
	if (key_match(store, key, key_len, hash))
		return hash;

	hash = pearson_hash2(key, key_len) % NUM_ENTRIES;
	if (key_match(store, key, key_len, hash))
		return hash;

	return -1;
}

int store_put(struct store *store,
		unsigned char *key, size_t key_len,
		unsigned char *value, size_t value_len)
{
	size_t req_blocks = floor_div(value_len, BLOCK_DATA_SIZE);
	uint64_t oldpos, datapos[req_blocks];
	int retcode = -1, hash;

	if (key_len > MAX_KEY_SIZE || value_len > MAX_VALUE_SIZE)
		return -1;

	if (req_blocks + 1 > NUM_BLOCKS)
		return -1;

	if (pthread_rwlock_wrlock(&store->rwlock))
		return -1;

	hash = put_hash(store, key, key_len);
	if (hash < 0)
		goto exit_unlock;

	// if a value already exists, we have to erase it
	if (store->keys[hash].length != 0) {
		oldpos = store->addresses[hash];
		erase_blocks(store, oldpos);
	}

	store->keys[hash].length = key_len;
	memcpy(store->keys[hash].data, key, key_len);

	if (allocate_blocks(store, datapos, req_blocks) < 0)
		goto exit_unlock;

	fill_blocks(store, value, value_len, datapos, req_blocks);
	retcode = 0;
exit_unlock:
	unlock_or_abort(&store->rwlock);
	return retcode;
}

int store_get(struct store *store, unsigned char *key, size_t key_len,
		unsigned char* dest, size_t dest_len)
{
	uint64_t valpos;
	struct block *blk;
	unsigned int blk_len;
	unsigned int actual_len = 0;
	int hash, retcode = -1;

	if (pthread_rwlock_rdlock(&store->rwlock))
		return -1;

	hash = get_hash(store, key, key_len);
	if (hash < 0)
		goto unlock_exit;

	valpos = store->addresses[hash];

	do {
		blk = &store->blocks[valpos];
		if (!block_is_occupied(blk))
			goto unlock_exit;
		blk_len = block_length_get(blk);
		if (blk_len > dest_len)
			goto unlock_exit;
		block_data_get(blk, dest);
		dest += blk_len;
		dest_len -= blk_len;
		actual_len += blk_len;
		valpos = block_ptr_get(blk);
	} while (!block_is_last(blk));

	retcode = actual_len;
unlock_exit:
	unlock_or_abort(&store->rwlock);
	return retcode;
}

int store_del(struct store *store, unsigned char *key, size_t key_len)
{
	uint64_t oldpos;
	int hash;
	int retcode = -1;

	if (pthread_rwlock_wrlock(&store->rwlock))
		return -1;

	hash = get_hash(store, key, key_len);
	if (hash < 0)
		goto unlock_exit;

	store->keys[hash].length = 0;
	oldpos = store->addresses[hash];
	erase_blocks(store, oldpos);
	retcode = 0;

unlock_exit:
	unlock_or_abort(&store->rwlock);
	return retcode;
}
