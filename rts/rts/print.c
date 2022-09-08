
#include "rts.h"

#include "mini-gmp.h"

void putS(const char *const s) {
  const char *l = s;
  while (*l) {
    putC(*l);
    l++;
  }
}

void putCAsString(char _c) {
  uint8_t c = (uint8_t)_c;

  const uint8_t r1 = c % 10;

  c = c / 10;
  if (c > 0) {
    const uint8_t r2 = c % 10;

    c = c / 10;
    if (c > 0) {
      const uint8_t r3 = c % 10;
      putC(r3 + 48);
      putC(r2 + 48);
      putC(r1 + 48);
    } else {
      putC(r2 + 48);
      putC(r1 + 48);
    }
  } else {
    putC(r1 + 48);
  }
}

void print(const struct NFData *data) {
  switch (data->type) {
  case IntegerType: {
    char *s = mpz_get_str(0, 10, data->value.integer.mpz);
    putS(s);
  } break;

  case BoolType: {
    if (data->value.boolean == 1) {
      putS("True");
    } else if (data->value.boolean == 0) {
      putS("False");
    } else {
      error_out();
    }
  } break;

  case UnitType: {
    putS("()");
  } break;

  case ByteStringType: {
    putC('{');

    const size_t length = data->value.byteString.length;

    if (length != 0) {
      for (size_t i = 0; i < length - 1; i++) {
        putCAsString(*(data->value.byteString.bytes + i));
        putC(',');
      }

      putCAsString(*(data->value.byteString.bytes + length - 1));
    }

    putC('}');
  } break;

  case PairType: {
    putC('(');
    print(data->value.pair.fst);
    putC(',');
    print(data->value.pair.snd);
    putC(')');
  } break;

  case ListType: {
    putC('[');
    const struct List *current = data->value.list;

    if (current) {
      print(current->elem);
      current = current->next;
    }

    while (current) {
      putC(',');
      print(current->elem);
      current = current->next;
    }
    putC(']');
  } break;

  case DataType: {
    enum DataSort s = data->value.data.sort;
    switch (s) {
    case ConstrS: {
      const nfdata_t *integerListPair =
          data->value.data.value.constr.integerListPair;

      putS("(Constr ");
      print(integerListPair->value.pair.fst);
      putC(' ');
      print(integerListPair->value.pair.snd);
      putC(')');
    } break;

    case MapS: {
      putS("(Map ");
      print(data->value.data.value.map.pairList);
      putC(')');
    } break;

    case ListS: {
      putS("(List ");
      print(data->value.data.value.list.list);
      putC(')');
    } break;

    case IntegerS: {
      putS("(Integer ");
      print(data->value.data.value.integer);
      putC(')');
    } break;

    case ByteStringS: {
      putS("(BS ");
      print(data->value.data.value.byteString);
      putC(')');
    } break;
    }
  } break;

  case TextType: {
    putS("Text");
  } break;

  default: {
    putS("Unhandeled (");
    putC(data->type);
    putC(')');
  } break;
  }
}