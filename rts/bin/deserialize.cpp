#include <rts.h>

#include <fstream>
#include <iostream>
#include <sstream>
#include <string>
#include <vector>

#include <stdint.h>

using namespace std;

// SIGABT (-6) on parsing failure

std::vector<char> readFile(string path) {
  ifstream ifd(path, ios::binary | ios::ate);
  int size = ifd.tellg();
  ifd.seekg(0, ios::beg);
  vector<char> buffer;
  buffer.resize(size);
  ifd.read(buffer.data(), size);

  if (buffer.size() % 4 != 0) {
    cout << "Serialized file size must be a multiple of 4" << endl;
    exit(1);
  }

  return buffer;
}

std::string to_string(const buffer_t &buffer) {
  if (buffer.len == 0) {
    return "[]";
  }

  std::stringstream ss;
  ss << "[";

  for (size_t i = 0; i < buffer.len - 1; i++) {
    ss << to_string(buffer.buf[i]) << ",";
  }

  ss << to_string(buffer.buf[buffer.len - 1]);

  ss << "]";

  return ss.str();
}

int main(int argc, char **argv) {
  if (argc != 2) {
    cout << "Pass serialized file path" << endl;
    return 1;
  }

  string path = argv[1];

  auto buffer = readFile(path);

  auto deserialized =
      deserialize_data(buffer_t{(uint32_t *)buffer.data(), buffer.size() / 4});

  print(deserialized.data);
  cout << endl;
  cout << to_string(deserialized.rest);

  return 0;
}
