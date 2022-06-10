#include "rts.h"

#include <string.h>

static uint8_t fromIntegral(const struct Integer *integer) {
  int v1 = mpz_get_si(integer->mpz);
  char v2 = v1;
  return (uint8_t)v2;
}

const struct NFData *
builtin_cons_bytestring__app_2(const struct LexicalScope *scope) {
  if (scope->first->type != ByteStringType) {
    error_out();
  }

  const struct Integer *integer = &scope->rest->first->value.integer;
  const struct ByteString *bs = &scope->first->value.byteString;

  const int newLength = bs->length + 1;
  uint8_t *buffer = (uint8_t *)alloc(newLength);

  const uint8_t v = fromIntegral(integer);

  *buffer = v;
  memcpy(buffer + 1, bs->bytes, bs->length);

  struct NFData *data = (struct NFData *)alloc(sizeof(struct NFData));
  data->type = ByteStringType;
  data->value.byteString.length = newLength;
  data->value.byteString.bytes = buffer;

  return data;
}

const struct NFData *
builtin_cons_bytestring__app_1(const struct LexicalScope *scope) {
  if (scope->first->type != IntegerType) {
    error_out();
  }

  struct NFData *data = (struct NFData *)alloc(sizeof(struct NFData));

  data->type = FunctionType;
  data->value.fn.apply = builtin_cons_bytestring__app_2;
  data->value.thunk.scope = scope;

  return data;
}

const struct NFData *builtin_cons_bytestring(const struct LexicalScope *scope) {
  struct NFData *data = (struct NFData *)alloc(sizeof(struct NFData));

  data->type = FunctionType;
  data->value.fn.apply = builtin_cons_bytestring__app_1;
  data->value.thunk.scope = scope;

  return data;
}
