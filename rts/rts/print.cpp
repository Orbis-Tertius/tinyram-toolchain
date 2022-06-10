
#include "rts.h"

#ifdef __tinyRAM__

void print(const struct NFData *data) { (void)data; }

#else

#include <cstdio>
#include <iomanip>
#include <ios>
#include <iostream>
#include <sstream>
#include <string>

#include "mini-gmp.h"

std::string _print(const struct NFData *data) {
  switch (data->type) {
  case IntegerType: {
    std::string r = mpz_get_str(nullptr, 10, data->value.integer.mpz);

    return r;
  }

  case BoolType: {
    if (data->value.boolean == 1) {
      return "True";
    } else if (data->value.boolean == 0) {
      return "False";
    } else {
      abort();
    }
  }

  case UnitType:
    return "()";

  case ByteStringType: {
    std::stringstream ss;
    ss << "{";

    const auto length = data->value.byteString.length;

    if (length != 0) {
      for (size_t i = 0; i < length - 1; i++) {
        ss << std::to_string(*(data->value.byteString.bytes + i));
        ss << ",";
      }

      ss << std::to_string(*(data->value.byteString.bytes + length - 1));
    }

    ss << "}";
    return ss.str();
  };

  case PairType: {
    std::stringstream ss;
    ss << "(";
    ss << _print(data->value.pair.fst);
    ss << ",";
    ss << _print(data->value.pair.snd);
    ss << ")";

    return ss.str();
  };

  case ListType: {
    std::stringstream ss;
    ss << "[";
    const struct List *current = data->value.list;

    if (current) {
      ss << _print(current->elem);
      current = current->next;
    }

    while (current) {
      ss << "," << _print(current->elem);
      current = current->next;
    }
    ss << "]";

    return ss.str();
  };

  case DataType: {
    return "Data";
  };

  case TextType: {
    return "Text";
  }

  default: {
    std::stringstream ss;
    ss << "Unhandeled (" << data->type << ")";
    return ss.str();
  }
  }
}

extern "C" void print(const struct NFData *data) { std::cout << _print(data); }

#endif