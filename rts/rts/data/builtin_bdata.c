#include "rts.h"

const struct NFData *builtin_bdata__app_1(const struct LexicalScope *scope) {
  if (scope->first->type != ByteStringType) {
    error_out();
  }

  struct NFData *data = (struct NFData *)alloc(sizeof(struct NFData));

  data->type = DataType;
  data->value.data.sort = ByteStringS;
  data->value.data.value.integer = scope->first;

  return data;
}

const struct NFData *builtin_bdata(const struct LexicalScope *scope) {
  struct NFData *data = (struct NFData *)alloc(sizeof(struct NFData));

  data->type = FunctionType;
  data->value.fn.apply = builtin_bdata__app_1;
  data->value.thunk.scope = scope;

  return data;
}
