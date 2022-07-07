#include "rts.h"

const struct NFData *
builtin_list_data__app_1(const struct LexicalScope *scope) {
  if (scope->first->type != ListType) {
    error_out();
  }

  // check that the list is of correct type

  struct NFData *data = (struct NFData *)alloc(sizeof(struct NFData));

  data->type = DataType;
  data->value.data.sort = ListS;
  data->value.data.value.list.list = scope->first;

  return data;
}

const struct NFData *builtin_list_data(const struct LexicalScope *scope) {
  struct NFData *data = (struct NFData *)alloc(sizeof(struct NFData));

  data->type = FunctionType;
  data->value.fn.apply = builtin_list_data__app_1;
  data->value.thunk.scope = scope;

  return data;
}
