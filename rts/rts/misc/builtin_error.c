#include "rts.h"

const struct NFData *builtin_error(const struct LexicalScope *scope) {
  error_out();

  return 0;
}
