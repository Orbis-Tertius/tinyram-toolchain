#include "special.h"

extern int main(void);

#define START_ATTR  __attribute__((used, __section__(".start_section")))

extern void data_and_rodata_section_init(void);

void START_ATTR start() {
  data_and_rodata_section_init();
  int result = main();
  if (result == 1) {
    exitCode(0);
  } else {
    exitCode(1);
  }
}

