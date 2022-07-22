/* 64 kB by default */
STACK_SIZE = DEFINED(__stack_size__) ? __stack_size__ : 0x10000;

SECTIONS
{
    .  = 0x0;

    .text : 
    {
	KEEP(*(.preamble_section))
	KEEP(*(.start_section))
        *(.text*)
    }

    . = ALIGN(8);
    .rodata (NOLOAD):
    {
        .rodata*
    }

    . = ALIGN(8);
    .data (NOLOAD):
    {
        .data*
    }

    . = ALIGN(8);
    .bss (NOLOAD):
    {
        .bss*
    }


   .stack (NOLOAD):
   {
      . = ALIGN(8);
      _sstack = .;
      . = . + STACK_SIZE;
      . = ALIGN(8);
      _estack = .;
   }
}
