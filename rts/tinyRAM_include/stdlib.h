#ifndef _STDLIB_H
#define _STDLIB_H	1

static void abort(void) {
  while (1) { }
}

static void *malloc(size_t size) {return 0;};
static void free(void *ptr) { };
static void *realloc(void *ptr, size_t size) {return 0;};

#endif
