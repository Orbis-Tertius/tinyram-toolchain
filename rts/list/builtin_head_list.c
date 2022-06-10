#include "rts.h"

const struct NFData *
builtin_head_list__app_1(const struct LexicalScope *scope) {
  if (scope->first->type != ListType) {
    diverge();
  }

  if (scope->first->value.list == 0) {
    diverge();
  }

  return scope->first->value.list->elem;
}

const struct NFData *
builtin_head_list__force_1(const struct LexicalScope *scope) {
  struct NFData *data = (struct NFData *)alloc(sizeof(struct NFData));

  data->type = FunctionType;
  data->value.fn.apply = builtin_head_list__app_1;
  data->value.fn.scope = scope;

  return data;
}

const struct NFData *builtin_head_list(const struct LexicalScope *scope) {
  struct NFData *data = (struct NFData *)alloc(sizeof(struct NFData));

  data->type = ThunkType;
  data->value.thunk.run = builtin_head_list__force_1;
  data->value.thunk.scope = scope;

  return data;
}