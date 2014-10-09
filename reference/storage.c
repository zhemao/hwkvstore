#include "storage.h"
#include "hash.h"

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <sys/mman.h>
#include <string.h>

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

	memcpy(data_start, dest, len);
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
	memcpy(src, data_start, len);
	return 0;
}

int store_init(struct store *store)
{
	void *ptr;

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

	return 0;
}

void store_cleanup(struct store *store)
{
	munmap((void *) store->blocks, BLOCK_SIZE * NUM_BLOCKS);
	close(store->fd);
}

void store_put(struct store *store, unsigned char *mess, size_t len)
{
}
