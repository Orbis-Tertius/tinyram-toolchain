#include "rts.h"

#include <string.h>

// TODO
// this is just a copy of bytestring append implementation
// can utf8 encoded strings be concatenated naively?

const struct NFData *
builtin_append_string__app_2(const struct LexicalScope *scope) {
  if (scope->first->type != TextType) {
    diverge();
  }

  const struct ByteString *bs1 = &scope->rest->first->value.byteString;
  const struct ByteString *bs2 = &scope->first->value.byteString;

  const int newLength = bs1->length + bs2->length;
  uint8_t *buffer = (uint8_t *)alloc(newLength);

  memcpy(buffer, bs1->bytes, bs1->length);
  memcpy(buffer + bs1->length, bs2->bytes, bs2->length);

  struct NFData *data = (struct NFData *)alloc(sizeof(struct NFData));
  data->type = TextType;
  data->value.byteString.length = newLength;
  data->value.byteString.bytes = buffer;

  return data;
}

const struct NFData *
builtin_append_string__app_1(const struct LexicalScope *scope) {
  if (scope->first->type != TextType) {
    diverge();
  }

  struct NFData *data = (struct NFData *)alloc(sizeof(struct NFData));

  data->type = FunctionType;
  data->value.fn.apply = builtin_append_string__app_2;
  data->value.thunk.scope = scope;

  return data;
}

const struct NFData *builtin_append_string(const struct LexicalScope *scope) {
  struct NFData *data = (struct NFData *)alloc(sizeof(struct NFData));

  data->type = FunctionType;
  data->value.fn.apply = builtin_append_string__app_1;
  data->value.thunk.scope = scope;

  return data;
}
