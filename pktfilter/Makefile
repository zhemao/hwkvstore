include Makefrag

TEST_FLAGS = --compile --genHarness --test --vcd --debug \
	--targetDir build/emulator/generated-src \
	--configInstance $(PROJ).$(CONFIG)
SCALA_SRC = src/main/scala/*.scala

filter-test:
	sbt "run-main $(PROJ).PacketFilterMain $(TEST_FLAGS)"

buffer-test:
	sbt "run-main $(PROJ).PacketBufferMain $(TEST_FLAGS)"
