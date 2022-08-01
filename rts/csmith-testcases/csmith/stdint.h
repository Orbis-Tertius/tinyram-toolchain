#ifndef _STDINT_H
#define _STDINT_H	1

typedef signed char		int8_t;
typedef short int		int16_t;
typedef int			int32_t;
#ifndef NO_LONGLONG
typedef long long int		int64_t;
#endif

/* Unsigned.  */
typedef unsigned char		uint8_t;
typedef unsigned short int	uint16_t;
typedef unsigned int		uint32_t;
#ifndef NO_LONGLONG
typedef unsigned long long int	uint64_t;
#endif


#define INT8_MIN    (-0x7f - 1)
#define INT16_MIN   (-0x7fff - 1)
#define INT32_MIN   (-0x7fffffff - 1)
#define INT64_MIN   (-0x7fffffffffffffff - 1)

#define INT8_MAX    0x7f
#define INT16_MAX   0x7fff
#define INT32_MAX   0x7fffffff
#define INT64_MAX   0x7fffffffffffffff

#define UINT8_MAX   0xff
#define UINT16_MAX  0xffff
#define UINT32_MAX  0xffffffff
#define UINT64_MAX  0xffffffffffffffff

#define CHAR_BIT 8

_Static_assert(sizeof(uint8_t) == 1, "uint8_t incorrect size");
_Static_assert(sizeof(uint16_t) == 2, "uint16_t incorrect size");
_Static_assert(sizeof(uint32_t) == 4, "uint32_t incorrect size");
_Static_assert(sizeof(uint64_t) == 8, "uint64_t incorrect size");

#endif
