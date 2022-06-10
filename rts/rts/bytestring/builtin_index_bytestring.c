#include "mini-gmp.h"
#include "rts.h"

#include <string.h>

const struct NFData *
builtin_index_bytestring__app_2(const struct LexicalScope *scope) {
  if (scope->first->type != IntegerType) {
    error_out();
  }

  const struct Integer *integer = &scope->first->value.integer;
  const struct ByteString *bs = &scope->rest->first->value.byteString;

  if (-1 == mpz_sgn(integer->mpz)) {
    error_out();
  }

  const unsigned long int index = mpz_get_ui(integer->mpz);

  if (index >= bs->length) {
    error_out();
  }

  struct NFData *data = (struct NFData *)alloc(sizeof(struct NFData));
  data->type = IntegerType;
  mpz_init(data->value.integer.mpz);
  mpz_set_ui(data->value.integer.mpz, bs->bytes[index]);

  return data;
}

const struct NFData *
builtin_index_bytestring__app_1(const struct LexicalScope *scope) {
  if (scope->first->type != ByteStringType) {
    error_out();
  }

  struct NFData *data = (struct NFData *)alloc(sizeof(struct NFData));

  data->type = FunctionType;
  data->value.fn.apply = builtin_index_bytestring__app_2;
  data->value.thunk.scope = scope;

  return data;
}

const struct NFData *
builtin_index_bytestring(const struct LexicalScope *scope) {
  struct NFData *data = (struct NFData *)alloc(sizeof(struct NFData));

  data->type = FunctionType;
  data->value.fn.apply = builtin_index_bytestring__app_1;
  data->value.thunk.scope = scope;

  return data;
}
