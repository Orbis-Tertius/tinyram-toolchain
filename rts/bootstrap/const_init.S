.section ".end_section"
.file   "const_init.s"
.globl  const_init
.align  32 #5
.type   const_init,@function
const_init:
  mov %r0, %r0 # NOP, passthrough
