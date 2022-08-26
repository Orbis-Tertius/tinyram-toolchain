#include <rts.h>

lexicalScope_t *alloc_lexicalScope() {
  return (lexicalScope_t *)alloc(sizeof(lexicalScope_t));
}

static buffer_t readBuffer(const uint32_t **bufPtr, const uint32_t *const end) {
  const uint32_t *addr = *bufPtr;
  const uint32_t len = *addr;
  addr += 1;
  if (addr + len > end) {
    error_out();
  }
  if (addr + len < addr) {
    error_out();
  }
  *bufPtr += len + 1;

  buffer_t buf = {addr, len};
  return buf;
}

static nfdata_t *readAndDeserialize(const uint32_t **bufPtr,
                                    const uint32_t *const end) {
  buffer_t buffer = readBuffer(bufPtr, end);
  deserialize_t d = deserialize_data(buffer);
  if (d.rest.len != 0) {
    error_out();
  }

  return d.data;
}

static const nfdata_t *apply(const nfdata_t *f, const nfdata_t *arg) {
  if (f->type != FunctionType) {
    error_out();
  }
  lexicalScope_t *s = alloc_lexicalScope();
  s->first = arg;
  s->rest = f->value.fn.scope;
  const nfdata_t *r = f->value.fn.apply(s);

  return r;
}

const nfdata_t *apply_script_args(func_t func) {
  tapes_t tapes = readTapes();

  const uint32_t *buf = tapes.publicT.buf;
  const uint32_t *const end = buf + tapes.publicT.len;

  nfdata_t *sc = readAndDeserialize(&buf, end);
  nfdata_t *datum = readAndDeserialize(&buf, end);
  nfdata_t *redeemer = readAndDeserialize(&buf, end);

  if (buf != end) {
    error_out();
  }

  const nfdata_t *f = func(0);
  const nfdata_t *f2 = apply(f, sc);
  const nfdata_t *f3 = apply(f2, datum);
  const nfdata_t *f4 = apply(f3, redeemer);

  return f4;
}
