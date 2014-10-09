LDFLAGS=-g
CFLAGS=-g

test: test.o hash.o storage.o
	$(CC) $(LDFLAGS) $^ -o $@

clean:
	rm -f testhash *.o
