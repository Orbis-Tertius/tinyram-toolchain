#include "rts.h"

const struct NFData *builtin_mk_cons__app_2(const struct LexicalScope *scope) {
  if (scope->first->type != ListType) {
    error_out();
  }

  // no check to prevent heterogenous lists yet

  const struct NFData *xs = scope->first;
  const struct NFData *x = scope->rest->first;

  struct List *new_list = (struct List *)alloc(sizeof(struct List));
  new_list->elem = x;
  new_list->next = xs->value.list;

  struct NFData *data = (struct NFData *)alloc(sizeof(struct NFData));
  data->type = ListType;
  data->value.list = new_list;

  return data;
}

const struct NFData *builtin_mk_cons__app_1(const struct LexicalScope *scope) {
  struct NFData *data = (struct NFData *)alloc(sizeof(struct NFData));

  data->type = FunctionType;
  data->value.fn.apply = builtin_mk_cons__app_2;
  data->value.fn.scope = scope;

  return data;
}

const struct NFData *
builtin_mk_cons__force_1(const struct LexicalScope *scope) {
  struct NFData *data = (struct NFData *)alloc(sizeof(struct NFData));

  data->type = FunctionType;
  data->value.fn.apply = builtin_mk_cons__app_1;
  data->value.fn.scope = scope;

  return data;
}

const struct NFData *builtin_mk_cons(const struct LexicalScope *scope) {
  struct NFData *data = (struct NFData *)alloc(sizeof(struct NFData));

  data->type = ThunkType;
  data->value.thunk.run = builtin_mk_cons__force_1;
  data->value.thunk.scope = scope;

  return data;
}