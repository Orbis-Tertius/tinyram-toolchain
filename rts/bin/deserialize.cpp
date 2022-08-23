#include <rts.h>

#include <fstream>
#include <iostream>
#include <string>
#include <vector>

#include <stdint.h>

using namespace std;

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

int main(int argc, char **argv) {
  if (argc != 2) {
    cout << "Pass serialized file path" << endl;
    return 1;
  }

  string path = argv[1];

  auto buffer = readFile(path);

  auto deserialized =
      deserialize_data(buffer_t{(uint32_t *)buffer.data(), buffer.size() / 4});

  if (deserialized.rest.len != 0) {
    cout << "Unexpected remnants at the end of buffer" << endl;
    return 1;
  }

  print(deserialized.data);

  return 0;
}
