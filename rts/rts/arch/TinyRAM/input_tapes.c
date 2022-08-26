#include <rts.h>
#include <stdint.h>

static const uint32_t *const lastWord = (const uint32_t *const)0xFFFFFFFC;

extern int _estack;

tapes_t readTapes() {
  const uint32_t *const estack = (uint32_t *)&_estack;

  const uint32_t *const publicLenAddr = lastWord;
  const uint32_t publicLen = *publicLenAddr;
  const uint32_t *publicBuf = publicLenAddr - publicLen;
  if (publicBuf <= estack || publicBuf > publicLenAddr) {
    error_out();
  }

  const uint32_t *const privateLenAddr = publicBuf - 1;
  const uint32_t privateLen = *privateLenAddr;
  const uint32_t *privateBuf = privateLenAddr - privateLen;
  if (privateBuf <= estack || privateBuf > privateLenAddr) {
    error_out();
  }

  tapes_t tapes;
  tapes.publicT.len = publicLen;
  tapes.publicT.buf = publicBuf;
  tapes.privateT.len = privateLen;
  tapes.privateT.buf = privateBuf;

  return tapes;
}
