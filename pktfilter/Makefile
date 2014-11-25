include Makefrag

TEST_FLAGS = --compile --genHarness --test --vcd --debug \
	--targetDir build/emulator/generated-src \
	--configInstance $(PROJ).$(CONFIG)
SCALA_SRC = src/main/scala/*.scala

filter-test:
	sbt "run-main $(PROJ).PacketFilterMain $(TEST_FLAGS)"

responder-test:
	sbt "run-main $(PROJ).ResponderMain $(TEST_FLAGS)"

checksum-test:
	sbt "run-main $(PROJ).ChecksumComputeMain $(TEST_FLAGS)"

buffer-test:
	sbt "run-main $(PROJ).PacketBufferMain $(TEST_FLAGS)"
