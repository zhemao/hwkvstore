#ifndef __ACCEL_INST_H__
#define __ACCEL_INST_H__

void fence(void);
void read_mode(void);
void write_mode(void);
unsigned long del_key(void *addr, unsigned long length);
unsigned long reserve_key(void *addr, unsigned int weight, unsigned int length);
void assoc_addr(unsigned long hash, unsigned long addr);
void assoc_len(unsigned long hash, unsigned long len);
void write_val(unsigned long hash, void *addr);

#endif
