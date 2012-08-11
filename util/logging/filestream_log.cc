#include "util.hh"
using namespace util::logging;

#include <fstream>
using namespace std;

filestream_log::filestream_log(ofstream &fstream) : stream_log(fstream), fstream(fstream) {}

void filestream_log::close() {
	fstream.close();
}

