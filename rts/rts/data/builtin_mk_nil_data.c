#include "rts.h"

const struct NFData *
builtin_mk_nil_data__app_1(const struct LexicalScope *scope) {
  if (scope->first->type != UnitType) {
    error_out();
  }

  struct NFData *data = (struct NFData *)alloc(sizeof(struct NFData));

  data->type = ListType;
  data->value.list = 0;

  return data;
}

const struct NFData *builtin_mk_nil_data(const struct LexicalScope *scope) {
  struct NFData *data = (struct NFData *)alloc(sizeof(struct NFData));

  data->type = FunctionType;
  data->value.fn.apply = builtin_mk_nil_data__app_1;
  data->value.thunk.scope = scope;

  return data;
}
