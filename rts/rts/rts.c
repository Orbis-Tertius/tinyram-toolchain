// uplc2c runtime system.
// This C code is to be included in the uplc2c compiler output for each program.
// This RTS is designed for compact code more so than performance or efficiency.

#include "./rts.h"

void diverge() {
  while (1) {
  }
}

/*****************************************************************************
 * Memory management
 *****************************************************************************/

unsigned char _heap[HEAP_SIZE]  __attribute__ ((aligned (4)));

unsigned char *heap_end = &_heap[0] + HEAP_SIZE;
unsigned char *heap_free = &_heap[0];

void *alloc(size_t bytes) {
  bytes = ((bytes + 3)/4)*4;

  if (heap_free + bytes < heap_end) {
    unsigned char *new_mem = heap_free;
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