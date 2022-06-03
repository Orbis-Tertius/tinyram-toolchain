#pragma once

struct TapeRead {
  int finished; // 1 if word consumed, 0 if end-of-tape
  int word;
};

void exitCode (int) __attribute__((noreturn))  __attribute__((naked));
long long int readTape(int tape) __attribute__((naked)) __attribute__((noinline));

