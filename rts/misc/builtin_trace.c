#include "rts.h"

const struct NFData *builtin_trace__app_2(const struct LexicalScope *scope) {
  print(scope->rest->first);

  return scope->first;
}

const struct NFData *builtin_trace__app_1(const struct LexicalScope *scope) {
  if (scope->first->type != TextType) {
    diverge();
  }

  struct NFData *data = (struct NFData *)alloc(sizeof(struct NFData));

  data->type = FunctionType;
  data->value.fn.apply = builtin_trace__app_2;
  data->value.fn.scope = scope;

  return data;
}

const struct NFData *builtin_trace__force_1(const struct LexicalScope *scope) {
  struct NFData *data = (struct NFData *)alloc(sizeof(struct NFData));

  data->type = FunctionType;
  data->value.fn.apply = builtin_trace__app_1;
  data->value.fn.scope = scope;

  return data;
}

const struct NFData *builtin_trace(const struct LexicalScope *scope) {
  struct NFData *data = (struct NFData *)alloc(sizeof(struct NFData));

  data->type = ThunkType;
  data->value.thunk.run = builtin_trace__force_1;
  data->value.thunk.scope = scope;

  return data;
}