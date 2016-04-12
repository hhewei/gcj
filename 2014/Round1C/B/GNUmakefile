CXX=clang++
CXXFLAGS=-std=c++11 -Wall -O2 -ggdb3
TARGETS=$(patsubst %.cpp,%,$(wildcard *.cpp))

all: ${TARGETS}

.PHONY: clean
clean:
	rm -f ${TARGETS}

