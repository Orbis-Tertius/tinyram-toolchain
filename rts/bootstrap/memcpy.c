#include <stdint.h>

#define size_t uint32_t

void * memcpy(void *dest, const void *src, unsigned int n)
{
   // Typecast src and dest addresses to (char *)
   char *csrc = (char *)src;
   char *cdest = (char *)dest;

   // Copy contents of src[] to dest[]
   if (cdest < csrc) {
     for (int i=0; i<n; i++) {
       cdest[i] = csrc[i];
     }
   } else if (csrc < cdest) {
     for (int i=n-1; i>=0; i--) {
       cdest[i] = csrc[i];
     }
   }

   return dest;
}

void * memset ( void * b, int c, size_t len ) {
  unsigned char *p = b;
  while(len > 0)
    {
      *p = c;
      p++;
      len--;
    }
  return b;
}
