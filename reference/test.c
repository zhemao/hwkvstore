#include "hash.h"
#include "storage.h"
#include <string.h>
#include <stdio.h>

int main(void) {
	/*char buffer[1024];
	uint64_t hash;
	struct block blk;
	unsigned int length = 1000;
	unsigned long ptr = 25L << 48 | 5;
	unsigned long res;*/

	/*memset(&blk, 0, sizeof(blk));
	block_length_set(&blk, length);
	block_ptr_set(&blk, ptr);

	res = block_length_get(&blk);
	if (res != length) {
		printf("Error: Expected %u but got %lu\n", length, res);
	}

	res = block_ptr_get(&blk);
	if (res != ptr) {
		printf("Error: Expected %lu but got %lu\n", ptr, res);
	}

	while (fgets(buffer, 1024, stdin) != NULL) {
		hash = pearson_hash(buffer, strlen(buffer));
		printf("%016lx\n", hash);
	}*/

	struct store store;
	unsigned char key[256] = "asdfasdfks";
	unsigned char value[2048];
	unsigned char result[2048];
	int i, key_len, value_len;
	int result_len;

	key_len = strlen((const char *) key);
	value_len = sizeof(value);

	for (i = 0; i < value_len; i++)
		value[i] = i % 256;

	if (store_init(&store) < 0) {
		fprintf(stderr, "Error on init\n");
		return -1;
	}

	if (store_put(&store, key, key_len, value, value_len) < 0) {
		fprintf(stderr, "Error on put\n");
		store_cleanup(&store);
		return -1;
	}

	result_len = store_get(&store, key, key_len, result, value_len);
	if (result_len != value_len) {
		fprintf(stderr, "Error on get: got len %d\n", result_len);
		store_cleanup(&store);
		return -1;
	}

	if (memcmp(value, result, value_len) != 0) {
		fprintf(stderr, "Error: incorrect results\n");
		store_cleanup(&store);
		return -1;
	}

	store_cleanup(&store);

	return 0;
}
