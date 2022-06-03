.section ".preamble_section"
.file   "preamble.s"
.globl  preamble
.align  32 #5
.type   preamble,@function
preamble:
  store.w 0, %r0
  mov %r0, 2147483648
.preamble1:
  read %r1, 0
  cjmp .preamble2
  add %r0, %r0, 4
  store.w %r0, %r1
  jmp .preamble1
.preamble2:
  store.w 2147483648, %r0
  mov %sp, _estack # set SP
  mov %fp, _estack # set FP
