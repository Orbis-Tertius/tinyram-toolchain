#ifndef RUNTIME_H
#define RUNTIME_H

#include <stdint.h>

static void platform_main_begin(void);
static void platform_main_begin(void)
{
}

static void platform_main_end(uint32_t crc, int flag) __attribute__((naked)) __attribute__((noinline));
static void platform_main_end(uint32_t crc, int flag)
{
  __asm__ ("answer %r0");
}

#define printf(...)
#define fprintf(...)

#endif
