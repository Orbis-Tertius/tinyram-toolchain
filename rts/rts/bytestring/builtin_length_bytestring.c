#include "mini-gmp.h"
#include "rts.h"

#include <string.h>

const struct NFData *
builtin_length_bytestring__app_1(const struct LexicalScope *scope) {
  if (scope->first->type != ByteStringType) {
    error_out();
  }

  const struct ByteString *bs = &scope->first->value.byteString;

  struct NFData *data = (struct NFData *)alloc(sizeof(struct NFData));
  data->type = IntegerType;
  mpz_init(data->value.integer.mpz);
  mpz_set_ui(data->value.integer.mpz, bs->length);

  return data;
}

const struct NFData *
builtin_length_bytestring(const struct LexicalScope *scope) {
  struct NFData *data = (struct NFData *)alloc(sizeof(struct NFData));

  data->type = FunctionType;
  data->value.fn.apply = builtin_length_bytestring__app_1;
  data->value.thunk.scope = scope;

  return data;
}
