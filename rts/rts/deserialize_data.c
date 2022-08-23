#include "mini-gmp.h"
#include <rts.h>

static const uint32_t constrType = 0;
static const uint32_t mapType = 1;
static const uint32_t listType = 2;
static const uint32_t integerType = 3;
static const uint32_t byteStringType = 4;

static nfdata_t *alloc_nfdata() { return (nfdata_t *)alloc(sizeof(nfdata_t)); }
static list_t *alloc_list() { return (list_t *)alloc(sizeof(list_t)); }

static buffer_t advance(buffer_t *buffer, size_t n) {
  buffer_t r = {buffer->buf + n, buffer->len - n};
  return r;
}

// fail if list has not even length
static nfdata_t *toPairList(nfdata_t *list) {
  nfdata_t *r = alloc_nfdata();
  r->type = ListType;
  r->value.list = 0;

  list_t *prev = 0;
  const list_t *l1 = list->value.list;
  while (l1) {
    const list_t *l2 = l1->next;
    if (l2 == 0) {
      error_out();
    }

    nfdata_t *pair = alloc_nfdata();
    pair->type = PairType;
    pair->value.pair.fst = l1->elem;
    pair->value.pair.snd = l2->elem;

    list_t *l = alloc_list();
    l->elem = pair;
    l->next = 0;

    if (prev) {
      prev->next = l;
    } else {
      r->value.list = l;
    }
    prev = l;

    l1 = l2->next;
  }

  return r;
}

static void splitAt(buffer_t *in, size_t at, buffer_t *out1, buffer_t *out2) {
  if (in->len < at) {
    out1->buf = in->buf;
    out1->len = in->len;

    out2->buf = 0;
    out2->len = 0;
  } else {
    out1->buf = in->buf;
    out1->len = at;

    out2->buf = in->buf + at;
    out2->len = in->len - at;
  }
}

static void deserialize_natural(mpz_t n, buffer_t buffer) {
  mpz_init(n);

  size_t i = 0;
  while (i < buffer.len) {
    uint32_t word = buffer.buf[buffer.len - i - 1];

    mpz_mul_2exp(n, n, 32);
    mpz_add_ui(n, n, word);

    i++;
  }
}

static deserialize_t deserialize_integer(buffer_t buffer) {
  if (buffer.len < 2) {
    error_out();
  }

  size_t len = buffer.buf[0];
  uint32_t sgn = buffer.buf[1];

  if (sgn != 0 && sgn != 1) {
    error_out();
  }

  if (buffer.len - 2 < len) {
    error_out();
  }

  buffer_t in = {buffer.buf + 2, buffer.len - 2};
  buffer_t out1, out2;
  splitAt(&in, len, &out1, &out2);

  nfdata_t *r = alloc_nfdata();
  r->type = IntegerType;
  mpz_t *n = &r->value.integer.mpz;

  deserialize_natural(*n, out1);
  if (sgn) {
    mpz_neg(*n, *n);
  }

  struct Deserialize res = {r, out2};

  return res;
}

static deserialize_t deserialize_list(buffer_t buffer) {
  if (buffer.len == 0) {
    error_out();
  }

  nfdata_t *r = alloc_nfdata();
  r->type = ListType;
  r->value.list = 0;

  size_t len = buffer.buf[0];

  size_t i = 0;
  buffer_t rest = {buffer.buf + 1, buffer.len - 1};
  list_t *prev = 0;
  while (i < len) {
    deserialize_t d = deserialize_data(rest);
    rest = d.rest;

    list_t *l = alloc_list();
    l->elem = d.data;
    l->next = 0;

    if (prev) {
      prev->next = l;
    } else {
      r->value.list = l;
    }
    prev = l;

    i++;
  }

  struct Deserialize res = {r, rest};

  return res;
}

static deserialize_t deserialize_bytestring(buffer_t buffer) {
  if (buffer.len == 0) {
    error_out();
  }

  size_t len = buffer.buf[0];

  size_t lenWords = ((len + 3) / 4);

  if (buffer.len - 1 < lenWords) {
    error_out();
  }

  nfdata_t *r = alloc_nfdata();
  r->type = ByteStringType;
  r->value.byteString.length = len;
  r->value.byteString.bytes = (uint8_t *)(buffer.buf + 1);

  struct Deserialize res = {
      r, {buffer.buf + 1 + lenWords, buffer.len - lenWords - 1}};

  return res;
}

static nfdata_t *constr(nfdata_t *integer, nfdata_t *list) {
  nfdata_t *pair = alloc_nfdata();

  pair->type = PairType;
  pair->value.pair.fst = integer;
  pair->value.pair.snd = list;

  nfdata_t *r = alloc_nfdata();

  r->type = DataType;
  r->value.data.sort = ConstrS;
  r->value.data.value.constr.integerListPair = pair;

  return r;
}

static nfdata_t *integer(nfdata_t *integer) {
  nfdata_t *r = alloc_nfdata();

  r->type = DataType;
  r->value.data.sort = IntegerS;
  r->value.data.value.integer = integer;

  return r;
}

static nfdata_t *list(nfdata_t *list) {
  nfdata_t *r = alloc_nfdata();

  r->type = DataType;
  r->value.data.sort = ListS;
  r->value.data.value.list.list = list;

  return r;
}

static nfdata_t *map(nfdata_t *pairList) {
  nfdata_t *r = alloc_nfdata();

  r->type = DataType;
  r->value.data.sort = MapS;
  r->value.data.value.map.pairList = pairList;

  return r;
}

static nfdata_t *byteString(nfdata_t *byteString) {
  nfdata_t *r = alloc_nfdata();

  r->type = DataType;
  r->value.data.sort = ByteStringS;
  r->value.data.value.byteString = byteString;

  return r;
}

deserialize_t deserialize_data(buffer_t buffer) {
  if (buffer.len == 0) {
    error_out();
  }

  const uint32_t x = buffer.buf[0];

  switch (x) {
  case constrType: {
    deserialize_t integerD = deserialize_integer(advance(&buffer, 1));
    deserialize_t listD = deserialize_list(integerD.rest);

    deserialize_t r = {constr(integerD.data, listD.data), listD.rest};

    return r;
  } break;

  case integerType: {
    deserialize_t integerD = deserialize_integer(advance(&buffer, 1));

    deserialize_t r = {integer(integerD.data), integerD.rest};

    return r;
  } break;

  case listType: {
    deserialize_t listD = deserialize_list(advance(&buffer, 1));

    deserialize_t r = {list(listD.data), listD.rest};

    return r;

  } break;

  case mapType: {
    deserialize_t listD = deserialize_list(advance(&buffer, 1));

    nfdata_t *pairList = toPairList(listD.data);

    deserialize_t r = {map(pairList), listD.rest};

    return r;
  } break;

  case byteStringType: {
    deserialize_t byteStringD = deserialize_bytestring(advance(&buffer, 1));

    deserialize_t r = {byteString(byteStringD.data), byteStringD.rest};

    return r;
  } break;

  default:
    error_out();
  }

  struct Deserialize r = {0, {0, 0}};

  return r;
}
