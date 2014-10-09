#include "hash.h"
#include "storage.h"
#include <string.h>
#include <stdio.h>

int main(void) {
	char buffer[1024];
	uint64_t hash;
	struct block blk;
	unsigned int length = 1000;
	unsigned long ptr = 25L << 48 | 5;
	unsigned long res;

	memset(&blk, 0, sizeof(blk));
	block_length_set(&blk, length);
	block_ptr_set(&blk, ptr);

	res = block_length_get(&blk);
	if (res != length) {
		printf("Error: Expected %u but got %u\n", length, res);
	}

	res = block_ptr_get(&blk);
	if (res != ptr) {
		printf("Error: Expected %lu but got %lu\n", ptr, res);
	}

	/*while (fgets(buffer, 1024, stdin) != NULL) {
		hash = pearson_hash(buffer, strlen(buffer));
		printf("%016lx\n", hash);
	}*/

	return 0;
}
