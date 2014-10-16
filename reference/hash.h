#ifndef REFERENCE_HASH
#define REFERENCE_HASH

#include <stdint.h>
#include <stdlib.h>

#define HASH_BYTES 2
typedef uint16_t hash_t;

hash_t pearson_hash1(const uint8_t *mess, size_t len);
hash_t pearson_hash2(const uint8_t *mess, size_t len);

#endif
