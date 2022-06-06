#include <stdint.h>

#include "mini-gmp.h"

#define WORD int32_t
#define WORD_BITS 32
#define DWORD int64_t
#define BOOL int

// The heap size. This may need to be adjusted on a per-program basis.
#define HEAP_SIZE (1024 * 16)

// We represent program failure by non-termination.
void diverge() __attribute__((noreturn));

/*****************************************************************************
 * Memory management
 *****************************************************************************/

// The first thing is a memory manager. This RTS uses a never-freed memory
// management model, meaning there is no manual memory manager and no GC;
// memory, once allocated, is never freed. This should work for our purposes
// where programs are expected to have short lifetimes.

// Technically, the heap is stored on the stack, and the main() function
// must set these global variables to point respectively to the end of the
// heap (heap_end) and the next free byte in the heap (heap_free);

extern unsigned char *heap_end;
extern unsigned char *heap_free;

// Allocates the given number of bytes of memory and returns a pointer to the
// beginning of the allocated memory.
void *alloc(WORD bytes);

/*****************************************************************************
 * Integers
 *****************************************************************************/

struct Integer {
  mpz_t mpz;
};

/*****************************************************************************
 * ByteStrings
 *****************************************************************************/

// This struct represents a Text or a ByteString depending on the type tag.
// Text is UTF8-encoded, so the EncodeUtf8 and DecodeUtf8 builtins are no-ops on
// the underlying ByteString; they only change the type tag.
struct ByteString {
  size_t length;
  const uint8_t *bytes;
};

/*****************************************************************************
 * Data
 *****************************************************************************/

enum DataSort { ConstrS, MapS, ListS, IntegerS, ByteStringS };

struct ConstrValue {
  const struct NFData *integerListPair; // (Integer, [Data])
};

struct MapValue {
  const struct NFData *pairList; // [(Data, Data)]
};

struct ListValue {
  const struct NFData *list; // [Data]
};

union DataValue {
  struct ConstrValue constr;
  struct MapValue map;
  struct ListValue list;
  const struct NFData *integer;
  const struct NFData *byteString;
};

struct Data {
  enum DataSort sort;
  union DataValue value;
};

/*****************************************************************************
 * NFData
 *****************************************************************************/

// The next thing is the representation of NFData, which is in other
// words data (or equivalently, code) which is in beta normal form.

enum NFDataType {
  FunctionType,
  IntegerType,
  UnitType,
  BoolType,
  ThunkType, // (result of Delay)
  TextType,
  ByteStringType,
  PairType,
  ListType,
  DataType
};

struct LexicalScope;

// The context is a LexicalScope and the output points
// to an NFData, but these are not in the type to avoid
// cyclic definitions.
typedef const struct NFData *(*Function)(const struct LexicalScope *context);

// A function is represented by a closure, which is a function pointer together
// with some data on the heap which it depends on. At this level of abstraction
// we do not distinguish between builtin and user-defined functions.
struct Closure {
  Function apply;
  const struct LexicalScope
      *scope; // may be null if the function does not dereference the pointer
};

typedef void (*Computation)(void *context);

// A thunk is effectively a closure over a function with no arguments.
struct Thunk {
  Function run;
  const struct LexicalScope *scope;
};

struct Pair {
  const struct NFData *fst;
  const struct NFData *snd;
};

struct List {
  const struct NFData *elem;
  const struct List *next;
};

union NFDataValue {
  struct Closure fn;
  struct Integer integer;
  int boolean;
  struct Thunk thunk;
  struct ByteString byteString;
  struct Pair pair;
  const struct List *list;
  struct Data data;
};

struct NFData {
  enum NFDataType type;
  union NFDataValue value;
};

/*****************************************************************************
 * Lexical scopes
 *****************************************************************************/

// A lexical scope is represented as a linked list of variable bindings,
// with the lower de Bruijn indices being first.
struct LexicalScope {
  const struct NFData *first;
  const struct LexicalScope *rest;
};

#ifdef __cplusplus
extern "C"
#endif
    void
    print(const struct NFData *data);

int compare_bytestring(const struct ByteString *bs1,
                       const struct ByteString *bs2);

#define DEF(name) const struct NFData *name(const struct LexicalScope *scope)

DEF(builtin_add_integer);
DEF(builtin_multiply_integer);
DEF(builtin_divide_integer);
DEF(builtin_equals_integer);
DEF(builtin_leq_integer);
DEF(builtin_less_integer);
DEF(builtin_mod_integer);
DEF(builtin_quotient_integer);
DEF(builtin_remainder_integer);
DEF(builtin_subtract_integer);

DEF(builtin_fst_pair);
DEF(builtin_snd_pair);

DEF(builtin_head_list);
DEF(builtin_tail_list);
DEF(builtin_null_list);
DEF(builtin_mk_cons);

DEF(builtin_if_then_else);
DEF(builtin_choose_unit);

DEF(builtin_constr_data);
DEF(builtin_map_data);
DEF(builtin_list_data);
DEF(builtin_idata);
DEF(builtin_bdata);
DEF(builtin_un_constr_data);
DEF(builtin_un_map_data);
DEF(builtin_un_list_data);
DEF(builtin_un_idata);
DEF(builtin_un_bdata);
DEF(builtin_mk_nil_data);
DEF(builtin_mk_nil_pair_data);
DEF(builtin_mk_pair_data);

DEF(builtin_append_bytestring);
DEF(builtin_cons_bytestring);
DEF(builtin_slice_bytestring);
DEF(builtin_length_bytestring);
DEF(builtin_index_bytestring);
DEF(builtin_equals_bytestring);
DEF(builtin_less_bytestring);
DEF(builtin_leq_bytestring);

DEF(builtin_append_string);
DEF(builtin_equals_string);
DEF(builtin_encode_utf8);
DEF(builtin_decode_utf8);

DEF(builtin_trace);