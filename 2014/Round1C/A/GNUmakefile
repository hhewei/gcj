CXX=clang++
CXXFLAGS=-Wall -ggdb3
TARGETS=$(patsubst %.cpp,%,$(wildcard *.cpp))

all: ${TARGETS}

.PHONY: clean
clean:
	rm -f ${TARGETS}

