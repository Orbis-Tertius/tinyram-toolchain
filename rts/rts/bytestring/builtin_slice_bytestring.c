#include "rts.h"
#include <string.h>

static size_t fromIntegral(const struct Integer *integer) {
  return mpz_get_ui(integer->mpz);
}

const struct NFData *
builtin_slice_bytestring__app_3(const struct LexicalScope *scope) {
  if (scope->first->type != ByteStringType) {
    diverge();
  }

  const struct Integer *start = &scope->rest->rest->first->value.integer;
  const struct Integer *n = &scope->rest->first->value.integer;
  const struct ByteString *bs = &scope->first->value.byteString;

  const size_t startU = -1 == mpz_sgn(start->mpz) ? 0 : fromIntegral(start);
  const size_t nU = -1 == mpz_sgn(n->mpz) ? 0 : fromIntegral(n);

  struct NFData *data = (struct NFData *)alloc(sizeof(struct NFData));
  data->type = ByteStringType;

  if (startU >= bs->length) {
    data->value.byteString.length = 0;
    data->value.byteString.bytes = 0;
  } else {
    const size_t left = bs->length - startU;
    const uint8_t *bytes = bs->bytes + startU;

    data->value.byteString.bytes = bytes;
    if (nU >= left) {
      data->value.byteString.length = left;
    } else {
      data->value.byteString.length = nU;
    }
  }

  return data;
}

const struct NFData *
builtin_slice_bytestring__app_2(const struct LexicalScope *scope) {
  if (scope->first->type != IntegerType) {
    diverge();
  }

  struct NFData *data = (struct NFData *)alloc(sizeof(struct NFData));

  data->type = FunctionType;
  data->value.fn.apply = builtin_slice_bytestring__app_3;
  data->value.thunk.scope = scope;

  return data;
}

const struct NFData *
builtin_slice_bytestring__app_1(const struct LexicalScope *scope) {
  if (scope->first->type != IntegerType) {
    diverge();
  }

  struct NFData *data = (struct NFData *)alloc(sizeof(struct NFData));

  data->type = FunctionType;
  data->value.fn.apply = builtin_slice_bytestring__app_2;
  data->value.thunk.scope = scope;

  return data;
}

const struct NFData *
builtin_slice_bytestring(const struct LexicalScope *scope) {
  struct NFData *data = (struct NFData *)alloc(sizeof(struct NFData));

  data->type = FunctionType;
  data->value.fn.apply = builtin_slice_bytestring__app_1;
  data->value.thunk.scope = scope;

  return data;
}
