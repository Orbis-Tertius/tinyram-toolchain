#include "rts.h"

#include <string.h>

const struct NFData *
builtin_encode_utf8__app_1(const struct LexicalScope *scope) {
  if (scope->first->type != TextType) {
    diverge();
  }

  const struct ByteString *bs = &scope->first->value.byteString;

  struct NFData *data = (struct NFData *)alloc(sizeof(struct NFData));

  data->type = ByteStringType;
  data->value.byteString.length = bs->length;
  data->value.byteString.bytes = bs->bytes;

  return data;
}

const struct NFData *builtin_encode_utf8(const struct LexicalScope *scope) {
  struct NFData *data = (struct NFData *)alloc(sizeof(struct NFData));

  data->type = FunctionType;
  data->value.fn.apply = builtin_encode_utf8__app_1;
  data->value.thunk.scope = scope;

  return data;
}
