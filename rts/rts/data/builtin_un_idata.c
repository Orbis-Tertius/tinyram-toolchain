#include "rts.h"

const struct NFData *builtin_un_idata__app_1(const struct LexicalScope *scope) {
  if (scope->first->type != DataType) {
    error_out();
  }

  if (scope->first->value.data.sort != IntegerS) {
    error_out();
  }

  return scope->first->value.data.value.integer;
}

const struct NFData *builtin_un_idata(const struct LexicalScope *scope) {
  struct NFData *data = (struct NFData *)alloc(sizeof(struct NFData));

  data->type = FunctionType;
  data->value.fn.apply = builtin_un_idata__app_1;
  data->value.thunk.scope = scope;

  return data;
}
