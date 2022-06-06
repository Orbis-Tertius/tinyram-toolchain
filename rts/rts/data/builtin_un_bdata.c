#include "rts.h"

const struct NFData *builtin_un_bdata__app_1(const struct LexicalScope *scope) {
  if (scope->first->type != DataType) {
    diverge();
  }

  if (scope->first->value.data.sort != ByteStringS) {
    diverge();
  }

  return scope->first->value.data.value.byteString;
}

const struct NFData *builtin_un_bdata(const struct LexicalScope *scope) {
  struct NFData *data = (struct NFData *)alloc(sizeof(struct NFData));

  data->type = FunctionType;
  data->value.fn.apply = builtin_un_bdata__app_1;
  data->value.thunk.scope = scope;

  return data;
}
