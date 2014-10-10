LDFLAGS=-g
CFLAGS=-g -Wall

test: test.o hash.o storage.o
	$(CC) $(LDFLAGS) $^ -o $@

clean:
	rm -f testhash *.o
