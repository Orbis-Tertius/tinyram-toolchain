#include "rts.h"

const struct NFData *
builtin_null_list__app_1(const struct LexicalScope *scope) {
  if (scope->first->type != ListType) {
    error_out();
  }

  struct NFData *data = (struct NFData *)alloc(sizeof(struct NFData));

  data->type = BoolType;
  data->value.boolean = (scope->first->value.list == 0);

  return data;
}

const struct NFData *
builtin_null_list__force_1(const struct LexicalScope *scope) {
  struct NFData *data = (struct NFData *)alloc(sizeof(struct NFData));

  data->type = FunctionType;
  data->value.fn.apply = builtin_null_list__app_1;
  data->value.fn.scope = scope;

  return data;
}

const struct NFData *builtin_null_list(const struct LexicalScope *scope) {
  struct NFData *data = (struct NFData *)alloc(sizeof(struct NFData));

  data->type = ThunkType;
  data->value.thunk.run = builtin_null_list__force_1;
  data->value.thunk.scope = scope;

  return data;
}