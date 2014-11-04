include Makefrag

vlsi-timestamp := build/vlsi/generated-src/timestamp

TEST_FLAGS = --compile --genHarness --test --vcd --debug \
	--targetDir build/emulator/generated-src \
	--configInstance $(PROJ).$(CONFIG)
VLSI_COMPILE_FLAGS = --compile --genHarness --debug --backend v \
	--targetDir build/vlsi/generated-src \
	--configInstance $(PROJ).$(CONFIG)
VLSI_RUN_FLAGS = --test --debug --backend v \
	--targetDir build/vlsi/generated-src \
	--configInstance $(PROJ).$(CONFIG)

$(vlsi-timestamp):
	sbt "run-main McAccel.PerfTestMain $(VLSI_COMPILE_FLAGS)"
	date > $(vlsi-timestamp)

vlsi: $(vlsi-timestamp)

run-vlsi: $(vlsi-timestamp)
	sbt "run-main McAccel.PerfTestMain $(VLSI_RUN_FLAGS)"

perftest-test:
	sbt "run-main McAccel.PerfTestMain $(TEST_FLAGS)"

kvstore-test:
	sbt "run-main McAccel.KeyValueStoreMain $(TEST_FLAGS)"

memhandler-test:
	sbt "run-main McAccel.MemoryHandlerMain $(TEST_FLAGS)"

lookup-test:
	sbt "run-main McAccel.LookupPipelineMain $(TEST_FLAGS)"

pearson-test:
	sbt "run-main McAccel.PearsonHasherMain $(TEST_FLAGS)"

keycompare-test:
	sbt "run-main McAccel.KeyCompareMain $(TEST_FLAGS)"

hasherwriter-test:
	sbt "run-main McAccel.HasherWriterMain $(TEST_FLAGS)"

hasherwriter-vlsi:
	sbt "run-main McAccel.HasherWriterMain $(VLSI_COMPILE_FLAGS)"
	sbt "run-main McAccel.HasherWriterMain $(VLSI_RUN_FLAGS)"
