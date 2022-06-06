#include "rts.h"

#include <string.h>

int compare_bytestring(const struct ByteString *bs1,
                       const struct ByteString *bs2) {
  if (bs1->length != bs2->length) {
    return 0;
  } else {
    for (size_t i = 0; i < bs1->length; i++) {
      if (bs1->bytes[i] != bs2->bytes[i]) {
        return 0;
      }
    }
  }

  return 1;
}

const struct NFData *
builtin_equals_bytestring__app_2(const struct LexicalScope *scope) {
  if (scope->first->type != ByteStringType) {
    diverge();
  }

  const struct ByteString *bs1 = &scope->rest->first->value.byteString;
  const struct ByteString *bs2 = &scope->first->value.byteString;

  struct NFData *data = (struct NFData *)alloc(sizeof(struct NFData));
  data->type = BoolType;
  data->value.boolean = compare_bytestring(bs1, bs2);

  return data;
}

const struct NFData *
builtin_equals_bytestring__app_1(const struct LexicalScope *scope) {
  if (scope->first->type != ByteStringType) {
    diverge();
  }

  struct NFData *data = (struct NFData *)alloc(sizeof(struct NFData));

  data->type = FunctionType;
  data->value.fn.apply = builtin_equals_bytestring__app_2;
  data->value.thunk.scope = scope;

  return data;
}

const struct NFData *
builtin_equals_bytestring(const struct LexicalScope *scope) {
  struct NFData *data = (struct NFData *)alloc(sizeof(struct NFData));

  data->type = FunctionType;
  data->value.fn.apply = builtin_equals_bytestring__app_1;
  data->value.thunk.scope = scope;

  return data;
}
