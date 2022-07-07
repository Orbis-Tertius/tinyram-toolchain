#include "rts.h"

#include <string.h>

const struct NFData *
builtin_equals_string__app_2(const struct LexicalScope *scope) {
  if (scope->first->type != TextType) {
    error_out();
  }

  const struct ByteString *bs1 = &scope->rest->first->value.byteString;
  const struct ByteString *bs2 = &scope->first->value.byteString;

  struct NFData *data = (struct NFData *)alloc(sizeof(struct NFData));
  data->type = BoolType;
  data->value.boolean = compare_bytestring(bs1, bs2);

  return data;
}

const struct NFData *
builtin_equals_string__app_1(const struct LexicalScope *scope) {
  if (scope->first->type != TextType) {
    error_out();
  }

  struct NFData *data = (struct NFData *)alloc(sizeof(struct NFData));

  data->type = FunctionType;
  data->value.fn.apply = builtin_equals_string__app_2;
  data->value.thunk.scope = scope;

  return data;
}

const struct NFData *builtin_equals_string(const struct LexicalScope *scope) {
  struct NFData *data = (struct NFData *)alloc(sizeof(struct NFData));

  data->type = FunctionType;
  data->value.fn.apply = builtin_equals_string__app_1;
  data->value.thunk.scope = scope;

  return data;
}
