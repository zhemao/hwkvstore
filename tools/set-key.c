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
	int retcode;

	if (argc < 4) {
		fprintf(stderr, "Usage: %s key value addr\n", argv[0]);
		return -1;
	}

	key = argv[1];
	value = argv[2];
	keylen = strlen(key);
	vallen = strlen(value);
	accel_addr = atoi(argv[3]);

	fence();
	write_mode();

	hash = accel_set(key, keylen, value, vallen, accel_addr);
	if (hash == NOT_FOUND) {
		fprintf(stderr, "Couldn't find a place for key\n");
		retcode = -1;
	} else {
		printf("Set key at hash %lu\n", hash);
		retcode = 0;
	}

	read_mode();
	fence();

	return retcode;
}
