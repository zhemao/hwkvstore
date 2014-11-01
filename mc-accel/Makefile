TEST_FLAGS = --compile --genHarness --test --vcd --debug --targetDir build/emulator/generated-src

perftest:
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
