/* 64 kB by default */
STACK_SIZE = DEFINED(__stack_size__) ? __stack_size__ : 0x10000;

SECTIONS
{
    .  = 0x0;

    .text : 
    {
	*(.preamble_section)
	*(.start_section)
        *(.text)
    }

    .rodata :
    {
        *(.rodata)
    }

    .data :
    {
        *(.data)
    }

    .bss :
    {
        *(.bss)
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
