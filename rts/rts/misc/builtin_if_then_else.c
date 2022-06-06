#include "rts.h"

const struct NFData *
builtin_if_then_else__app_3(const struct LexicalScope *scope) {
  const struct LexicalScope *s = scope;

  const struct NFData *b = s->first;
  s = s->rest;

  const struct NFData *a = s->first;
  s = s->rest;

  const struct NFData *cond = s->first;

  if (cond->type != BoolType) {
    diverge();
  }

  // should check that 'a' and 'b' have the same types

  if (cond->value.boolean == 1) {
    return a;
  } else {
    return b;
  }
}

const struct NFData *
builtin_if_then_else__app_2(const struct LexicalScope *scope) {
  struct NFData *data = (struct NFData *)alloc(sizeof(struct NFData));

  data->type = FunctionType;
  data->value.fn.apply = builtin_if_then_else__app_3;
  data->value.fn.scope = scope;

  return data;
}

const struct NFData *
builtin_if_then_else__app_1(const struct LexicalScope *scope) {
  struct NFData *data = (struct NFData *)alloc(sizeof(struct NFData));

  data->type = FunctionType;
  data->value.fn.apply = builtin_if_then_else__app_2;
  data->value.fn.scope = scope;

  return data;
}

const struct NFData *
builtin_if_then_else__force_1(const struct LexicalScope *scope) {
  struct NFData *data = (struct NFData *)alloc(sizeof(struct NFData));

  data->type = FunctionType;
  data->value.fn.apply = builtin_if_then_else__app_1;
  data->value.fn.scope = scope;

  return data;
}

const struct NFData *builtin_if_then_else(const struct LexicalScope *scope) {
  struct NFData *data = (struct NFData *)alloc(sizeof(struct NFData));

  data->type = ThunkType;
  data->value.thunk.run = builtin_if_then_else__force_1;
  data->value.thunk.scope = scope;

  return data;
}
