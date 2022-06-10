#include "rts.h"

const struct NFData *
builtin_choose_list__app_3(const struct LexicalScope *scope) {
  const struct LexicalScope *s = scope;

  const struct NFData *v1 = s->first;
  s = s->rest;

  const struct NFData *v2 = s->first;
  s = s->rest;

  const struct NFData *list = s->first;

  if (list->type != ListType) {
    error_out();
  }

  // no type safety yet (type v1 == type v2)

  if (list->value.list == 0) {
    return v2;
  } else {
    return v1;
  }
}

const struct NFData *
builtin_choose_list__app_2(const struct LexicalScope *scope) {
  struct NFData *data = (struct NFData *)alloc(sizeof(struct NFData));

  data->type = FunctionType;
  data->value.fn.apply = builtin_choose_list__app_3;
  data->value.fn.scope = scope;

  return data;
}

const struct NFData *
builtin_choose_list__app_1(const struct LexicalScope *scope) {
  struct NFData *data = (struct NFData *)alloc(sizeof(struct NFData));

  data->type = FunctionType;
  data->value.fn.apply = builtin_choose_list__app_2;
  data->value.fn.scope = scope;

  return data;
}

const struct NFData *
builtin_choose_list__force_2(const struct LexicalScope *scope) {
  struct NFData *data = (struct NFData *)alloc(sizeof(struct NFData));

  data->type = FunctionType;
  data->value.fn.apply = builtin_choose_list__app_1;
  data->value.fn.scope = scope;

  return data;
}

const struct NFData *
builtin_choose_list__force_1(const struct LexicalScope *scope) {
  struct NFData *data = (struct NFData *)alloc(sizeof(struct NFData));

  data->type = ThunkType;
  data->value.thunk.run = builtin_choose_list__force_2;
  data->value.thunk.scope = scope;

  return data;
}

const struct NFData *builtin_choose_list(const struct LexicalScope *scope) {
  struct NFData *data = (struct NFData *)alloc(sizeof(struct NFData));

  data->type = ThunkType;
  data->value.thunk.run = builtin_choose_list__force_1;
  data->value.thunk.scope = scope;

  return data;
}
