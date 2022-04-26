#include "rts.h"

static int compare_integers(const struct Integer *d1,
                            const struct Integer *d2) {
  return (mpz_cmp(d1->mpz, d2->mpz) == 0);
}

static int compare_data(const struct Data *d1, const struct Data *d2);

static int compare_data_list(const struct List *l1, const struct List *l2) {
  if (l1 == 0 && l2 == 0) {
    return 1;
  }

  if (l1 == 0 || l2 == 0) {
    return 0;
  }

  if (0 == compare_data(&l1->elem->value.data, &l2->elem->value.data)) {
    return 0;
  }

  return compare_data_list(l1->next, l2->next);
}

static int compare_pair_list(const struct List *l1, const struct List *l2) {
  if (l1 == 0 && l2 == 0) {
    return 1;
  }

  if (l1 == 0 || l2 == 0) {
    return 0;
  }

  const struct Pair *p1 = &l1->elem->value.pair;
  const struct Pair *p2 = &l2->elem->value.pair;

  if (0 == compare_data(&p1->fst->value.data, &p2->fst->value.data)) {
    return 0;
  }

  if (0 == compare_data(&p1->snd->value.data, &p2->snd->value.data)) {
    return 0;
  }

  return compare_pair_list(l1->next, l2->next);
}

static int compare_data(const struct Data *d1, const struct Data *d2) {
  if (d1->sort != d2->sort) {
    return 0;
  }

  switch (d1->sort) {
  case ConstrS: {
    const struct Pair *p1 = &d1->value.constr.integerListPair->value.pair;
    const struct Pair *p2 = &d2->value.constr.integerListPair->value.pair;

    const struct Integer *i1 = &p1->fst->value.integer;
    const struct Integer *i2 = &p2->fst->value.integer;

    if (0 == compare_integers(i1, i2)) {
      return 0;
    }

    const struct List *l1 = p1->snd->value.list;
    const struct List *l2 = p2->snd->value.list;

    if (0 == compare_data_list(l1, l2)) {
      return 0;
    }

    return 1;
  }
  case MapS: {
    const struct List *l1 = d1->value.map.pairList->value.list;
    const struct List *l2 = d2->value.map.pairList->value.list;

    if (0 == compare_pair_list(l1, l2)) {
      return 0;
    }

    return 1;
  }
  case ListS: {
    const struct List *l1 = d1->value.list.list->value.list;
    const struct List *l2 = d2->value.list.list->value.list;

    if (0 == compare_data_list(l1, l2)) {
      return 0;
    }

    return 1;
  }
  case IntegerS: {
    const struct Integer *i1 = &d1->value.integer->value.integer;
    const struct Integer *i2 = &d2->value.integer->value.integer;
    return (0 == compare_integers(i1, i2));
  }
  case ByteStringS: {
    const struct ByteString *bs1 = &d1->value.byteString->value.byteString;
    const struct ByteString *bs2 = &d2->value.byteString->value.byteString;
    return compare_bytestring(bs1, bs2);
  }
  }

  return 0;
}

const struct NFData *
builtin_equals_data__app_2(const struct LexicalScope *scope) {
  if (scope->first->type != DataType) {
    diverge();
  }

  struct NFData *data = (struct NFData *)alloc(sizeof(struct NFData));

  data->type = BoolType;
  data->value.boolean = 0;

  return data;
}

const struct NFData *
builtin_equals_data__app_1(const struct LexicalScope *scope) {
  if (scope->first->type != DataType) {
    diverge();
  }

  struct NFData *data = (struct NFData *)alloc(sizeof(struct NFData));

  data->type = FunctionType;
  data->value.fn.apply = builtin_equals_data__app_2;
  data->value.thunk.scope = scope;

  return data;
}

const struct NFData *builtin_equals_data(const struct LexicalScope *scope) {
  struct NFData *data = (struct NFData *)alloc(sizeof(struct NFData));

  data->type = FunctionType;
  data->value.fn.apply = builtin_equals_data__app_1;
  data->value.thunk.scope = scope;

  return data;
}
