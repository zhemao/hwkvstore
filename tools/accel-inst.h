#ifndef __ACCEL_INST_H__
#define __ACCEL_INST_H__

static inline void fence(void) {
	asm volatile ("fence");
}

static inline void read_mode(void) {
	asm volatile ("custom0 0, 0, 0, 0");
}

static inline void write_mode(void) {
	asm volatile ("custom0 0, 1, 0, 0");
}

static inline unsigned long del_key(void *addr, unsigned long length) {
	unsigned long result;
	asm volatile ("custom0 %[result], %[addr], %[length], 1"
			: [result] "=r" (result)
			: [addr] "r" (addr), [length] "r" (length));
	return result;
}

static inline unsigned long reserve_key(
		void *addr, unsigned int weight, unsigned int length) {
	unsigned long wl_comb = weight << 8 | (length & 0xff);
	unsigned long result;
	asm volatile ("custom0 %[result], %[addr], %[wl], 2"
			: [result] "=r" (result)
			: [addr] "r" (addr), [wl] "r" (wl_comb));
	return result;
}

static inline void assoc_addr(unsigned long hash, unsigned long addr) {
	asm volatile ("custom0 0, %[hash], %[addr], 3" :
			: [hash] "r" (hash), [addr] "r" (addr));
}

static inline void assoc_len(unsigned long hash, unsigned long len) {
	asm volatile ("custom0 0, %[hash], %[len], 4" :
			: [hash] "r" (hash), [len] "r" (len));
}

static inline void write_val(unsigned long hash, void *addr) {
	asm volatile ("custom0 0, %[hash], %[addr], 5" :
			: [hash] "r" (hash), [addr] "r" (addr));
}

#endif
