#include "accel-inst.h"
#include "accel.h"

unsigned long accel_set(
	void *key, unsigned int keylen, void *value, unsigned int vallen,
	unsigned long accel_addr)
{
	unsigned long hash;

	hash = reserve_key(key, 0, keylen);
	if (hash == NOT_FOUND)
		return hash;

	assoc_addr(hash, accel_addr);
	assoc_len(hash, vallen);
	write_val(hash, value);

	return hash;
}
