#include "util.h"

#include <fstream>
#include <iostream>
#include <sstream>

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