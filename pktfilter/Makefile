include Makefrag

TEST_FLAGS = --compile --genHarness --test --vcd --debug \
	--targetDir build/emulator/generated-src \
	--configInstance $(PROJ).$(CONFIG)
SCALA_SRC = src/main/scala/*.scala

filter-test:
	$(SBT) "run-main $(PROJ).PacketFilterMain $(TEST_FLAGS)"

responder-test:
	$(SBT) "run-main $(PROJ).ResponderMain $(TEST_FLAGS)"

checksum-test:
	$(SBT) "run-main $(PROJ).ChecksumComputeMain $(TEST_FLAGS)"

buffer-test:
	$(SBT) "run-main $(PROJ).PacketBufferMain $(TEST_FLAGS)"

packet-test:
	$(SBT) "run-main $(PROJ).PacketTestMain $(TEST_FLAGS)"
