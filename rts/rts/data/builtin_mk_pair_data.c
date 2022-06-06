#include "rts.h"

const struct NFData *
builtin_mk_pair_data__app_2(const struct LexicalScope *scope) {
  if (scope->first->type != DataType) {
    diverge();
  }

  struct NFData *data = (struct NFData *)alloc(sizeof(struct NFData));

  data->type = PairType;
  data->value.pair.fst = scope->rest->first;
  data->value.pair.snd = scope->first;

  return data;
}

const struct NFData *
builtin_mk_pair_data__app_1(const struct LexicalScope *scope) {
  if (scope->first->type != DataType) {
    diverge();
  }

  struct NFData *data = (struct NFData *)alloc(sizeof(struct NFData));

  data->type = FunctionType;
  data->value.fn.apply = builtin_mk_pair_data__app_2;
  data->value.thunk.scope = scope;

  return data;
}

const struct NFData *builtin_mk_pair_data(const struct LexicalScope *scope) {
  struct NFData *data = (struct NFData *)alloc(sizeof(struct NFData));

  data->type = FunctionType;
  data->value.fn.apply = builtin_mk_pair_data__app_1;
  data->value.thunk.scope = scope;

  return data;
}
