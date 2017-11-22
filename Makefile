
.PHONY: debug
debug:
	sbt 'testOnly hbwif2.HBWIFTester --'
