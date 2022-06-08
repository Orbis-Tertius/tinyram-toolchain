#ifndef _STRING_H
#define _STRING_H	1

void *memcpy(void *dest, const void *src, unsigned int n);

int memcmp(const void *ptr1, const void *ptr2, size_t num);

static int isspace (int c) {
  return c == ' ' || c == '\t' || c == '\n';
}

static size_t strlen ( const char * str ) {
    size_t size = 0;

    while (*str != 0) {
        ++size;
        str++;
    }

    return size;
}

#endif
