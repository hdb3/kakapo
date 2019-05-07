TARGET = relay
LIBS = -lm
CC = gcc
CFLAGS = -g -pthread -D_GNU_SOURCE

.PHONY: default all clean

default: $(TARGET)
all: default

OBJECTS = $(patsubst %.c, %.o, $(wildcard *.c))
HEADERS = $(wildcard *.h)

%.o: %.c $(HEADERS)
	$(CC) $(CFLAGS) -c $< -o $@

.PRECIOUS: $(TARGET) $(OBJECTS)

$(TARGET): $(OBJECTS)
	$(CC) $(OBJECTS) -pthread -Wall $(LIBS) -o $@

clean:
	-rm -f *.o
	-rm -f $(TARGET)
