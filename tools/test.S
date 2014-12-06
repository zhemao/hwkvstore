
  .text
  .global _start
_start:
  la t0, exception
  csrw evec, t0
  li t0, 0x100
  csrs status, t0
  csrr t0, status
  // switch to write mode
  custom0 0, 1, 0, 0
  // reserve key and get hash value
  li t0, 0x2094
  li t1, 5
  custom0 t2, t0, t1, 2
  // assoc addr 0
  li t0, 0
  custom0 0, t2, t0, 3
  // assoc len 7
  li t0, 7
  custom0 0, t2, t0, 4
  // write value
  li t0, 0x209a
  custom0 0, t2, t0, 5
  // switch to read mode
  custom0 0, 0, 0, 0

  csrw tohost, 1
1: b 1b

  nop
  nop
  nop
  nop
  nop
  nop
  nop

exception:
  csrr t0, epc
  addi t0, t0, 4
  csrw epc, t0
  csrr t1, cause
  sret

  nop
  nop
  nop
  nop
  nop
  nop

key:
  .string "a key"
value:
  .string "a value"
