#include "rts.h"

const struct NFData *
builtin_tail_list__app_1(const struct LexicalScope *scope) {
  if (scope->first->type != ListType) {
    error_out();
  }

  if (scope->first->value.list == 0) {
    error_out();
  }

  struct NFData *data = (struct NFData *)alloc(sizeof(struct NFData));

  data->type = ListType;
  data->value.list = scope->first->value.list->next;

  return data;
}

const struct NFData *
builtin_tail_list__force_1(const struct LexicalScope *scope) {
  struct NFData *data = (struct NFData *)alloc(sizeof(struct NFData));

  data->type = FunctionType;
  data->value.fn.apply = builtin_tail_list__app_1;
  data->value.fn.scope = scope;

  return data;
}

const struct NFData *builtin_tail_list(const struct LexicalScope *scope) {
  struct NFData *data = (struct NFData *)alloc(sizeof(struct NFData));

  data->type = ThunkType;
  data->value.thunk.run = builtin_tail_list__force_1;
  data->value.thunk.scope = scope;

  return data;
}