#include "rts.h"

#include <string.h>

static uint8_t fromIntegral(const struct Integer *integer) {
  unsigned long int z = mpz_get_ui(integer->mpz);
  return (uint8_t)(z & 0xff);
}

const struct NFData *
builtin_cons_bytestring__app_2(const struct LexicalScope *scope) {
  if (scope->first->type != ByteStringType) {
    diverge();
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
    diverge();
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
