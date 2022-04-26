#include "rts.h"

#include <string.h>

enum Ordering { LT, EQ, GT };

// https://hackage.haskell.org/package/bytestring-0.11.3.1/docs/src/Data.ByteString.Internal.html#compareBytes
static enum Ordering compare(const struct ByteString *bs1,
                             const struct ByteString *bs2) {
  if (bs1->length == 0 && bs2->length == 0) {
    return EQ;
  }

  size_t min = bs1->length < bs2->length ? bs1->length : bs2->length;

  const int r = memcmp(bs1->bytes, bs2->bytes, min);

  if (r > 0) {
    return GT;
  } else if (r < 0) {
    return LT;
  } else {
    return EQ;
  }
}

// less_bytestring

const struct NFData *
builtin_less_bytestring__app_2(const struct LexicalScope *scope) {
  if (scope->first->type != ByteStringType) {
    diverge();
  }

  const struct ByteString *bs1 = &scope->rest->first->value.byteString;
  const struct ByteString *bs2 = &scope->first->value.byteString;

  const enum Ordering o = compare(bs1, bs2);

  struct NFData *data = (struct NFData *)alloc(sizeof(struct NFData));
  data->type = BoolType;
  data->value.boolean = o == LT;

  return data;
}

const struct NFData *
builtin_less_bytestring__app_1(const struct LexicalScope *scope) {
  if (scope->first->type != ByteStringType) {
    diverge();
  }

  struct NFData *data = (struct NFData *)alloc(sizeof(struct NFData));

  data->type = FunctionType;
  data->value.fn.apply = builtin_less_bytestring__app_2;
  data->value.thunk.scope = scope;

  return data;
}

const struct NFData *builtin_less_bytestring(const struct LexicalScope *scope) {
  struct NFData *data = (struct NFData *)alloc(sizeof(struct NFData));

  data->type = FunctionType;
  data->value.fn.apply = builtin_less_bytestring__app_1;
  data->value.thunk.scope = scope;

  return data;
}

// leq bytestring

const struct NFData *
builtin_leq_bytestring__app_2(const struct LexicalScope *scope) {
  if (scope->first->type != ByteStringType) {
    diverge();
  }

  const struct ByteString *bs1 = &scope->rest->first->value.byteString;
  const struct ByteString *bs2 = &scope->first->value.byteString;

  const enum Ordering o = compare(bs1, bs2);

  struct NFData *data = (struct NFData *)alloc(sizeof(struct NFData));
  data->type = BoolType;
  data->value.boolean = o == LT || o == EQ;

  return data;
}

const struct NFData *
builtin_leq_bytestring__app_1(const struct LexicalScope *scope) {
  if (scope->first->type != ByteStringType) {
    diverge();
  }

  struct NFData *data = (struct NFData *)alloc(sizeof(struct NFData));

  data->type = FunctionType;
  data->value.fn.apply = builtin_leq_bytestring__app_2;
  data->value.thunk.scope = scope;

  return data;
}

const struct NFData *builtin_leq_bytestring(const struct LexicalScope *scope) {
  struct NFData *data = (struct NFData *)alloc(sizeof(struct NFData));

  data->type = FunctionType;
  data->value.fn.apply = builtin_leq_bytestring__app_1;
  data->value.thunk.scope = scope;

  return data;
}
