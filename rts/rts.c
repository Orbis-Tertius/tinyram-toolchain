// uplc2c runtime system.
// This C code is to be included in the uplc2c compiler output for each program.
// This RTS is designed for compact code more so than performance or efficiency.

#include <stdio.h>

#include "./rts.h"

void diverge() {
  while (1) {
  }
}

/*****************************************************************************
 * Memory management
 *****************************************************************************/

unsigned char _heap[HEAP_SIZE];

unsigned char *heap_end = &_heap[0] + HEAP_SIZE;
unsigned char *heap_free = &_heap[0];

void *alloc(WORD bytes) {
  if (heap_free + bytes < heap_end) {
    void *new_mem = heap_free;
    heap_free += bytes;
    return new_mem;
  } else {
    diverge();
  }

  return 0;
}

/*****************************************************************************
 * NFData
 *****************************************************************************/