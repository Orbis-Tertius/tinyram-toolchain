// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

// This implementation was taken from https://github.com/llvm/llvm-project/tree/main/compiler-rt

#include <stdint.h>

typedef int32_t si_int;
typedef uint32_t su_int;
typedef int32_t fixint_t;
typedef uint32_t fixuint_t;

#define CHAR_BIT 8

#define COMPUTE_UDIV(a, b) ((su_int)(a) / (su_int)(b))

fixint_t __divsi3(fixint_t a, fixint_t b) {
  const int N = (int)(sizeof(fixint_t) * CHAR_BIT) - 1;
  fixint_t s_a = a >> N;                            // s_a = a < 0 ? -1 : 0
  fixint_t s_b = b >> N;                            // s_b = b < 0 ? -1 : 0
  fixuint_t a_u = (fixuint_t)(a ^ s_a) + (-s_a);    // negate if s_a == -1
  fixuint_t b_u = (fixuint_t)(b ^ s_b) + (-s_b);    // negate if s_b == -1
  s_a ^= s_b;                                       // sign of quotient
  return (COMPUTE_UDIV(a_u, b_u) ^ s_a) + (-s_a);   // negate if s_a == -1
}

si_int __modsi3(si_int a, si_int b) {
  return a - __divsi3(a, b) * b;
}



