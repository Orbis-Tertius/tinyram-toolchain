#include "rts.h"

const struct NFData *
builtin_choose_data__app_6(const struct LexicalScope *scope) {
  const struct LexicalScope *current = scope;
  const struct NFData *op6 = current->first;

  current = current->rest;
  const struct NFData *op5 = current->first;

  current = current->rest;
  const struct NFData *op4 = current->first;

  current = current->rest;
  const struct NFData *op3 = current->first;

  current = current->rest;
  const struct NFData *op2 = current->first;

  current = current->rest;
  const struct NFData *op1 = current->first;

  switch (op1->value.data.sort) {
  case ConstrS:
    return op2;
  case MapS:
    return op3;
  case ListS:
    return op4;
  case IntegerS:
    return op5;
  case ByteStringS:
    return op6;
  }

  return 0;
}

const struct NFData *
builtin_choose_data__app_5(const struct LexicalScope *scope) {
  struct NFData *data = (struct NFData *)alloc(sizeof(struct NFData));

  data->type = FunctionType;
  data->value.fn.apply = builtin_choose_data__app_6;
  data->value.thunk.scope = scope;

  return data;
}

const struct NFData *
builtin_choose_data__app_4(const struct LexicalScope *scope) {
  if (scope->first->type != DataType) {
    diverge();
  }

  struct NFData *data = (struct NFData *)alloc(sizeof(struct NFData));

  data->type = FunctionType;
  data->value.fn.apply = builtin_choose_data__app_5;
  data->value.thunk.scope = scope;

  return data;
}

const struct NFData *
builtin_choose_data__app_3(const struct LexicalScope *scope) {
  struct NFData *data = (struct NFData *)alloc(sizeof(struct NFData));

  data->type = FunctionType;
  data->value.fn.apply = builtin_choose_data__app_4;
  data->value.thunk.scope = scope;

  return data;
}

const struct NFData *
builtin_choose_data__app_2(const struct LexicalScope *scope) {
  struct NFData *data = (struct NFData *)alloc(sizeof(struct NFData));

  data->type = FunctionType;
  data->value.fn.apply = builtin_choose_data__app_3;
  data->value.thunk.scope = scope;

  return data;
}

const struct NFData *
builtin_choose_data__app_1(const struct LexicalScope *scope) {
  if (scope->first->type != DataType) {
    diverge();
  }

  struct NFData *data = (struct NFData *)alloc(sizeof(struct NFData));

  data->type = FunctionType;
  data->value.fn.apply = builtin_choose_data__app_2;
  data->value.thunk.scope = scope;

  return data;
}

const struct NFData *builtin_choose_data(const struct LexicalScope *scope) {
  struct NFData *data = (struct NFData *)alloc(sizeof(struct NFData));

  data->type = FunctionType;
  data->value.fn.apply = builtin_choose_data__app_1;
  data->value.thunk.scope = scope;

  return data;
}
