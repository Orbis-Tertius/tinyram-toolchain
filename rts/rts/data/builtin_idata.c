#include "rts.h"

const struct NFData *builtin_idata__app_1(const struct LexicalScope *scope) {
  if (scope->first->type != IntegerType) {
    error_out();
  }

  struct NFData *data = (struct NFData *)alloc(sizeof(struct NFData));

  data->type = DataType;
  data->value.data.sort = IntegerS;
  data->value.data.value.integer = scope->first;

  return data;
}

const struct NFData *builtin_idata(const struct LexicalScope *scope) {
  struct NFData *data = (struct NFData *)alloc(sizeof(struct NFData));

  data->type = FunctionType;
  data->value.fn.apply = builtin_idata__app_1;
  data->value.thunk.scope = scope;

  return data;
}
