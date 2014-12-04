#include "accel-inst.h"

void fence(void) {
	asm volatile ("fence");
}

void read_mode(void) {
	asm volatile ("custom0 0, 0, 0, 0");
}

void write_mode(void) {
	asm volatile ("custom0 0, 1, 0, 0");
}

unsigned long del_key(void *addr, unsigned long length) {
	unsigned long result;
	asm volatile ("custom0 %[result], %[addr], %[length], 1"
			: [result] "=r" (result)
			: [addr] "r" (addr), [length] "r" (length));
	return result;
}

unsigned long reserve_key(void *addr, unsigned int weight, unsigned int length) {
	unsigned long wl_comb = weight << 8 | (length & 0xff);
	unsigned long result;
	asm volatile ("custom0 %[result], %[addr], %[wl], 2"
			: [result] "=r" (result)
			: [addr] "r" (addr), [wl] "r" (wl_comb));
	return result;
}

void assoc_addr(unsigned long hash, unsigned long addr) {
	asm volatile ("custom0 0, %[hash], %[addr], 3" :
			: [hash] "r" (hash), [addr] "r" (addr));
}

void assoc_len(unsigned long hash, unsigned long len) {
	asm volatile ("custom0 0, %[hash], %[len], 4" :
			: [hash] "r" (hash), [len] "r" (len));
}

void write_val(unsigned long hash, void *addr) {
	asm volatile ("custom0 0, %[hash], %[addr], 5" :
			: [hash] "r" (hash), [addr] "r" (addr));
}
