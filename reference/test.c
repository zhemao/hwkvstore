#include "hash.h"
#include "storage.h"
#include <string.h>
#include <stdio.h>

int main(void) {
	struct store store;
	unsigned char key[256] = "asdfklj;kadgjaskn23kgnas";
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

	if (store_del(&store, key, key_len) < 0) {
		fprintf(stderr, "Problem deleting\n");
		store_cleanup(&store);
		return -1;
	}

	if (store_get(&store, key, key_len, result, value_len) >= 0) {
		fprintf(stderr, "Error: unexpected data found\n");
		store_cleanup(&store);
		return -1;
	}

	if (store_put(&store, key, key_len, result, value_len) < 0) {
		fprintf(stderr, "Error on second put\n");
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
		fprintf(stderr, "Error: second incorrect results\n");
		store_cleanup(&store);
		return -1;
	}

	for (i = 0; i < value_len; i++)
		value[i] = 255 - (i % 256);

	if (store_put(&store, key, key_len, value, value_len) < 0) {
		fprintf(stderr, "Error on third put\n");
		store_cleanup(&store);
		return -1;
	}

	result_len = store_get(&store, key, key_len, result, value_len);
	if (result_len != value_len) {
		fprintf(stderr, "Error on third get: got len %d\n", result_len);
		store_cleanup(&store);
		return -1;
	}

	if (memcmp(value, result, value_len) != 0) {
		fprintf(stderr, "Error: third incorrect results\n");
		store_cleanup(&store);
		return -1;
	}

	store_cleanup(&store);
	return 0;
}
