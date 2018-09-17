
.PHONY: test
test:
	futhark-test lib/github.com/diku-dk/statistics/statistics_tests.fut

.PHONY: test
clean:
	rm -rf *~
