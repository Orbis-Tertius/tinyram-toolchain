#include "special.h"

extern int main(void);

#define START_ATTR  __attribute__((used, __section__(".start_section")))

void START_ATTR start() {
  int result = main();
  if (result == 1) {
    exitCode(0);
  } else {
    exitCode(1);
  }
}

