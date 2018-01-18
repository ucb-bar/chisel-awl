
.PHONY: debug
debug:
	sbt 'testOnly hbwif2.HBWIFTester --'

.PHONY: clean
clean:
	rm -rf project/project project/target target ucli.key test_run_dir DVEfiles
