#include <rts.h>

#include <iostream>
#include <stdlib.h>
#include <string.h>

#include "util.h"

tapes_t readTapes() {
  char *path = getenv("PUBLIC_TAPE_FILEPATH");

  if (!path) {
    std::cerr << "Input tape file path not specified" << std::endl;
    exit(1);
  }

  auto vec = readFile(path);

  auto buf = (uint32_t *)alloc(vec.size());

  memcpy(buf, vec.data(), vec.size());

  tapes_t tapes;
  tapes.privateT.buf = 0;
  tapes.privateT.len = 0;
  tapes.publicT.buf = buf;
  tapes.publicT.len = vec.size() / 4;

  return tapes;
}
