#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "hash.h"

int main(int argc, char *argv[]) {
	int hash_alg;
	uint8_t key[256];
	size_t keylen;
	hash_t hash;

	if (argc < 3) {
		fprintf(stderr, "Usage: %s alg key\n", argv[0]);
		return -1;
	}

	hash_alg = atoi(argv[1]);
	if (hash_alg < 1 || hash_alg > 2) {
		fprintf(stderr, "Hash algorithm must be 1 or 2\n");
		return -1;
	}

	keylen = strlen(argv[2]);
	if (keylen >= 256) {
		fprintf(stderr, "Key must be smaller than 256 bytes\n");
		return -1;
	}
	memcpy(key, argv[2], keylen);
	key[keylen] = 0;

	if (hash_alg == 1)
		hash = pearson_hash1(key, keylen);
	else
		hash = pearson_hash2(key, keylen);
	hash &= 0x3ff;

	printf("Hash: %x\n", hash);

	return 0;
}
