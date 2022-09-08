.section ".preamble_section"
.file   "preamble.s"
.globl  preamble
.align  32 #5
.type   preamble,@function
preamble:
  mov %lr, .return1
  jmp const_init
.return1:
  mov %sp, _estack # set SP
  mov %fp, _estack # set FP
