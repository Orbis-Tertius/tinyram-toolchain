#include "rts.h"

const struct NFData *builtin_fst_pair__app_1(const struct LexicalScope *scope) {
  if (scope->first->type != PairType) {
    error_out();
  }

  return scope->first->value.pair.fst;
}

const struct NFData *
builtin_fst_pair__force_2(const struct LexicalScope *scope) {
  struct NFData *data = (struct NFData *)alloc(sizeof(struct NFData));

  data->type = FunctionType;
  data->value.fn.apply = builtin_fst_pair__app_1;
  data->value.thunk.scope = scope;

  return data;
}

const struct NFData *
builtin_fst_pair__force_1(const struct LexicalScope *scope) {
  struct NFData *data = (struct NFData *)alloc(sizeof(struct NFData));

  data->type = ThunkType;
  data->value.thunk.run = builtin_fst_pair__force_2;
  data->value.thunk.scope = scope;

  return data;
}

const struct NFData *builtin_fst_pair(const struct LexicalScope *scope) {
  struct NFData *data = (struct NFData *)alloc(sizeof(struct NFData));

  data->type = ThunkType;
  data->value.thunk.run = builtin_fst_pair__force_1;
  data->value.thunk.scope = scope;

  return data;
}
