
.PHONY: test
test:
	futhark-test lib/github.com/diku-dk/statistics/statistics_tests.fut lib/github.com/diku-dk/statistics/gammaln_tests.fut

.PHONY: sync
sync:
	futhark-pkg sync

.PHONY: test
clean:
	rm -rf *~
