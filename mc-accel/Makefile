include Makefrag

vlsi-timestamp := build/vlsi/generated-src/timestamp

TEST_FLAGS = --compile --genHarness --test --vcd --debug \
	--targetDir build/emulator/generated-src \
	--configInstance $(PROJ).$(CONFIG)
VLSI_COMPILE_FLAGS = --compile --genHarness --debug --backend v \
	--targetDir $(VLSI_OUT_DIR) \
	--configInstance $(PROJ).$(CONFIG) \
	--noInlineMem
VLSI_RUN_FLAGS = --test --debug --backend v \
	--targetDir build/vlsi/generated-src \
	--configInstance $(PROJ).$(CONFIG)

SCALA_SRC = src/main/scala/*.scala

$(vlsi-timestamp): $(SCALA_SRC)
	sbt "run-main McAccel.PerfTestMain $(VLSI_COMPILE_FLAGS)"
	$(vlsi_mem_gen) $(VLSI_OUT_DIR)/KeyValueStore.$(CONFIG).conf \
		>> $(VLSI_OUT_DIR)/KeyValueStore.$(CONFIG).v
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

bankedmem-test:
	sbt "run-main McAccel.BankedMemMain $(TEST_FLAGS)"

valuecache-test:
	sbt "run-main McAccel.ValueCacheMain $(TEST_FLAGS)"

hasherwriter-vlsi:
	sbt "run-main McAccel.HasherWriterMain $(VLSI_COMPILE_FLAGS)"

lookup-vlsi:
	sbt "run-main McAccel.LookupPipelineMain $(VLSI_COMPILE_FLAGS)"

dc-syn: $(vlsi-timestamp)
	make -C build/vlsi/dc-syn SHELL=/bin/bash
