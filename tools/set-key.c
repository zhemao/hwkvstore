#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "accel-inst.h"
#include "accel.h"

#define MAX_VALUE_SIZE 1422

int main(int argc, char *argv[])
{
	char *key, *value;
	unsigned int keylen, vallen;
	unsigned long accel_addr, hash;

	if (argc < 3) {
		fprintf(stderr, "Usage: %s key [value] addr\n", argv[0]);
		return -1;
	}

	key = argv[1];
	keylen = strlen(key);

	if (argc < 4) {
		int n;
		value = alloca(MAX_VALUE_SIZE + 1);
		n = fread(value, 1, MAX_VALUE_SIZE, stdin);
		if (n < 0) {
			perror("fread");
			return -1;
		}
		value[n] = '\0';
		vallen = n;
		accel_addr = atoi(argv[2]);
	} else {
		value = argv[2];
		vallen = strlen(value);
		accel_addr = atoi(argv[3]);
	}

	write_mode();
	fence();

	hash = accel_set(key, keylen, value, vallen, accel_addr);
	if (hash == NOT_FOUND) {
		fprintf(stderr, "Couldn't find a place for key\n");
		return -1;
	}

	printf("Set key at hash %lu\n", hash);
	return 0;
}
