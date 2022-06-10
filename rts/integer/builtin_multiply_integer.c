#include "rts.h"

const struct NFData *
builtin_multiply_integer__partially_app_2(const struct LexicalScope *scope) {
  if (scope->first->type != IntegerType) {
    diverge();
  }

  struct NFData *op2 = (struct NFData *)scope->first;
  struct NFData *op1 = (struct NFData *)scope->rest->first;

  struct NFData *data = (struct NFData *)alloc(sizeof(struct NFData));

  data->type = IntegerType;

  mpz_init(data->value.integer.mpz);
  mpz_mul(data->value.integer.mpz, op1->value.integer.mpz,
          op2->value.integer.mpz);

  return data;
}

const struct NFData *
builtin_multiply_integer__partially_app_1(const struct LexicalScope *scope) {
  if (scope->first->type != IntegerType) {
    diverge();
  }

  struct NFData *data = (struct NFData *)alloc(sizeof(struct NFData));

  data->type = FunctionType;
  data->value.fn.apply = &builtin_multiply_integer__partially_app_2;
  data->value.fn.scope = scope;

  return data;
}

const struct NFData *
builtin_multiply_integer(const struct LexicalScope *scope) {
  struct NFData *data = (struct NFData *)alloc(sizeof(struct NFData));

  data->type = FunctionType;
  data->value.fn.apply = &builtin_multiply_integer__partially_app_1;
  data->value.fn.scope = scope;

  return data;
}
