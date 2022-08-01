#ifndef RUNTIME_H
#define RUNTIME_H

#include <stdint.h>

int printf ( const char * format, ... );

static void platform_main_begin(void);
static void platform_main_begin(void)
{
}

static void platform_main_end(uint32_t crc, int flag) __attribute__((noinline));
static void platform_main_end(uint32_t crc, int flag)
{
  printf("Answer: 0x%x\n", crc);
}

#endif
