#include "special.h"

void exitCode(int code)  {
  __asm__("answer %r0");
}


// CC is based on implicit contract between llvm frontent and backend
// long long int is 64 bits and its returned via pair of registers: r0 and r1
long long int readTape(int tape)  {
  __asm__("read %r1, %r0 \n"
          "mov %r0, 0    \n"
	  "cmov %r0, 1   \n"
	  "jmp %lr         ");
}

void example() {
  long long int r2 = readTape(1);
  struct TapeRead* r = (struct TapeRead*)&r2;
  int isFinished9 = r->finished == 9;
  int isWord11 = r->word == 11;
  if (isFinished9 && isWord11) {
    exitCode(5);
  } else {
    exitCode(6);
  }
}
