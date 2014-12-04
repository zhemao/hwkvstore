#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "accel-inst.h"
#include "accel.h"

int main(int argc, char *argv[])
{
	char *key, *value;
	unsigned int keylen, vallen;
	unsigned long accel_addr, hash;

	if (argc < 4) {
		fprintf(stderr, "Usage: %s key value addr\n", argv[0]);
		return -1;
	}

	key = argv[1];
	value = argv[2];
	keylen = strlen(key);
	vallen = strlen(value);
	accel_addr = atoi(argv[3]);

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
