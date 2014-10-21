TEST_FLAGS = --compile --genHarness --test --vcd --debug --targetDir build/emulator/generated-src

pearson-test:
	sbt "run-main McAccel.PearsonHasherMain $(TEST_FLAGS)"

keycompare-test:
	sbt "run-main McAccel.KeyCompareMain $(TEST_FLAGS)"

hasherwriter-test:
	sbt "run-main McAccel.HasherWriterMain $(TEST_FLAGS)"
