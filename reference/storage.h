#ifndef REFERENCE_STORAGE
#define REFERENCE_STORAGE

#include <stdint.h>
#include <stdlib.h>
#include <pthread.h>

#define BLOCK_SIZE 1024
#define BLOCK_DATA_SIZE (1024 - 9)
#define LEN_SIZE 10
#define ADDR_SIZE 54
#define ADDR_SIZE_BYTES 7
#define STATUS_KEY 7
#define STATUS_LAST 6
#define STATUS_OCCUPIED 5
#define MAX_KEY_SIZE (BLOCK_DATA_SIZE - ADDR_SIZE_BYTES)
#define MAX_VALUE_SIZE (4 * 1024 * 1024)

struct block {
	unsigned char status;
	/* We use a char array instead of uint64_t so that no padding is
	 * inserted. */
	unsigned char lenptr[8];
	unsigned char data[BLOCK_DATA_SIZE];
};

#define NUM_ENTRIES 1024
#define NUM_BLOCKS (1 << 16)
#define HASH_FUNC(msg, len) pearson_hash(msg, len)
#define NO_ADDR 0xffffffff

struct store {
	uint64_t entries[NUM_ENTRIES];
	struct block *blocks;
	pthread_rwlock_t rwlock;
	int fd;
};

int store_init(struct store *store);
void store_cleanup(struct store *store);
int store_put(struct store *store,
		unsigned char *key, size_t key_len,
		unsigned char *value, size_t value_len);
int store_get(struct store *store, unsigned char *key, size_t key_len,
		unsigned char* dest, size_t dest_len);
int store_del(struct store *store, unsigned char *key, size_t key_len);

#endif
