#ifndef __ACCEL_H__
#define __ACCEL_H__

#define NOT_FOUND 0xffffff

unsigned long accel_set(
	void *key, unsigned int keylen, void *value, unsigned int vallen,
	unsigned long accel_addr);

#endif
