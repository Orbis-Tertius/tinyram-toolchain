#ifndef _STDLIB_H
#define _STDLIB_H	1

#include <stddef.h>

static void abort(void) {
  while (1) { }
}

extern void *alloc(size_t bytes);
extern void * memcpy ( void * destination, const void * source, size_t num );

static void *malloc(size_t size) { return alloc(size); };
static void free(void *ptr) { };
static void *realloc(void *ptr, size_t size) {
  void * new = malloc(size);
  memcpy(new, ptr, size);
  return new;
};

#endif
