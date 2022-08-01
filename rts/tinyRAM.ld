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
	KEEP(*(.end_section))
    }

    . = ALIGN(8);
    .rodata :
    {
        .rodata*
    }

    . = ALIGN(8);
    .data :
    {
        .data*
    }

    . = ALIGN(8);
    .bss :
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
