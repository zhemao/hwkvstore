include Makefrag

vlsi-timestamp := build/vlsi/generated-src/timestamp
fpga-timestamp := build/fpga/generated-src/timestamp

TEST_FLAGS = --compile --genHarness --test --vcd --debug \
	--targetDir build/emulator/generated-src \
	--configInstance $(PROJ).$(EMU_CONFIG)
VLSI_COMPILE_FLAGS = --compile --genHarness --debug --backend v \
	--targetDir $(VLSI_OUT_DIR) \
	--configInstance $(PROJ).$(VLSI_CONFIG) \
	--noInlineMem
VLSI_RUN_FLAGS = --test --debug --backend v \
	--targetDir build/vlsi/generated-src \
	--configInstance $(PROJ).$(VLSI_CONFIG)
FPGA_COMPILE_FLAGS = --compile --backend fpga \
	--targetDir build/fpga/generated-src \
	--configInstance $(PROJ).$(FPGA_CONFIG)

SCALA_SRC = src/main/scala/*.scala
SRAMS = build/sram/*.conf

$(fpga-timestamp): $(SCALA_SRC)
	$(SBT) "run-main $(PROJ).KeyValueStoreMain $(FPGA_COMPILE_FLAGS)"

fpga: $(fpga-timestamp)

$(vlsi-timestamp): $(SCALA_SRC) $(SRAMS) $(vlsi_mem_gen)
	$(SBT) "run-main $(PROJ).PerfTestMain $(VLSI_COMPILE_FLAGS)"
	$(vlsi_mem_gen) $(VLSI_OUT_DIR)/KeyValueStore.$(VLSI_CONFIG).conf \
		>> $(VLSI_OUT_DIR)/KeyValueStore.$(VLSI_CONFIG).v
	date > $(vlsi-timestamp)

vlsi: $(vlsi-timestamp)

run-vlsi: $(vlsi-timestamp)
	$(SBT) "run-main $(PROJ).PerfTestMain $(VLSI_RUN_FLAGS)"

perftest-test:
	$(SBT) "run-main $(PROJ).PerfTestMain $(TEST_FLAGS)"

kvstore-test:
	$(SBT) "run-main $(PROJ).KeyValueStoreMain $(TEST_FLAGS)"

memhandler-test:
	$(SBT) "run-main $(PROJ).MemoryHandlerMain $(TEST_FLAGS)"

lookup-test:
	$(SBT) "run-main $(PROJ).LookupPipelineMain $(TEST_FLAGS)"

pearson-test:
	$(SBT) "run-main $(PROJ).PearsonHasherMain $(TEST_FLAGS)"

keycompare-test:
	$(SBT) "run-main $(PROJ).KeyCompareMain $(TEST_FLAGS)"

hasherwriter-test:
	$(SBT) "run-main $(PROJ).HasherWriterMain $(TEST_FLAGS)"

bankedmem-test:
	$(SBT) "run-main $(PROJ).BankedMemMain $(TEST_FLAGS)"

valuecache-test:
	$(SBT) "run-main $(PROJ).ValueCacheMain $(TEST_FLAGS)"

hasherwriter-vlsi:
	$(SBT) "run-main $(PROJ).HasherWriterMain $(VLSI_COMPILE_FLAGS)"

lookup-vlsi:
	$(SBT) "run-main $(PROJ).LookupPipelineMain $(VLSI_COMPILE_FLAGS)"

dc-syn: $(vlsi-timestamp)
	make -C build/vlsi/dc-syn SHELL=/bin/bash

icc-par: $(vlsi-timestamp)
	make -C build/vlsi/icc-par SHELL=/bin/bash
