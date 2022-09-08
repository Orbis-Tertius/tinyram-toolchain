.section ".text"
.file   "putC.s"
.globl  putC
.align  32 #5
.type   putC,@function
putC:
  out %r0
  jmp %lr
