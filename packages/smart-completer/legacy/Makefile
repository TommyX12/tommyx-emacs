CC := g++
# -g means to have debug
# -I. means to find .h files in current directory
CFLAGS := -Wall -I.
# how to make different things depending on OS?
TARGET := smart-completer.exe

# $(wildcard *.cpp /xxx/xxx/*.cpp): get all .cpp files from the current directory and dir "/xxx/xxx/"
SRCS := $(wildcard *.cpp)
# $(patsubst %.cpp,%.o,$(SRCS)): substitute all ".cpp" file name strings to ".o" file name strings
DEPS := $(wildcard *.h)
OBJS := $(patsubst %.cpp,%.o,$(SRCS))

all: $(TARGET)
$(TARGET): $(OBJS)
	$(CC) -o $@ $^
%.o: %.cpp $(DEPS)
	$(CC) $(CFLAGS) -c $<
clean:
	rm -rf $(TARGET) *.o

# means 'all' and 'clean' don't generate files
.PHONY: all clean

