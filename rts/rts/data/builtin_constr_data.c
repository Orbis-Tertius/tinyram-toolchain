#include "rts.h"

const struct NFData *
builtin_constr_data__app_2(const struct LexicalScope *scope) {
  if (scope->first->type != ListType) {
    error_out();
  }

  // check that the list is of correct type

  struct NFData *data = (struct NFData *)alloc(sizeof(struct NFData));
  struct NFData *pair = (struct NFData *)alloc(sizeof(struct NFData));

  pair->type = PairType;
  pair->value.pair.fst = scope->rest->first;
  pair->value.pair.snd = scope->first;

  data->type = DataType;
  data->value.data.sort = ConstrS;
  data->value.data.value.constr.integerListPair = pair;

  return data;
}

const struct NFData *
builtin_constr_data__app_1(const struct LexicalScope *scope) {
  if (scope->first->type != IntegerType) {
    error_out();
  }

  struct NFData *data = (struct NFData *)alloc(sizeof(struct NFData));

  data->type = FunctionType;
  data->value.fn.apply = builtin_constr_data__app_2;
  data->value.thunk.scope = scope;

  return data;
}

const struct NFData *builtin_constr_data(const struct LexicalScope *scope) {
  struct NFData *data = (struct NFData *)alloc(sizeof(struct NFData));

  data->type = FunctionType;
  data->value.fn.apply = builtin_constr_data__app_1;
  data->value.thunk.scope = scope;

  return data;
}
