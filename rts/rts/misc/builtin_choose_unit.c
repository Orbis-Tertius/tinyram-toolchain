#include "rts.h"

const struct NFData *
builtin_choose_unit__app_2(const struct LexicalScope *scope) {
  return scope->first;
}

const struct NFData *
builtin_choose_unit__app_1(const struct LexicalScope *scope) {
  struct NFData *data = (struct NFData *)alloc(sizeof(struct NFData));

  data->type = FunctionType;
  data->value.fn.apply = builtin_choose_unit__app_2;
  data->value.fn.scope = scope;

  if (scope->first->type != UnitType) {
    error_out();
  }

  return data;
}

const struct NFData *
builtin_choose_unit__force_1(const struct LexicalScope *scope) {
  struct NFData *data = (struct NFData *)alloc(sizeof(struct NFData));

  data->type = FunctionType;
  data->value.fn.apply = builtin_choose_unit__app_1;
  data->value.fn.scope = scope;

  return data;
}

const struct NFData *builtin_choose_unit(const struct LexicalScope *scope) {
  struct NFData *data = (struct NFData *)alloc(sizeof(struct NFData));

  data->type = ThunkType;
  data->value.thunk.run = builtin_choose_unit__force_1;
  data->value.thunk.scope = scope;

  return data;
}