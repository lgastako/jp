help:
	@cat Makefile

deploy:
	lein deploy clojars

test-clj:
	lein test

test-cljs:
	lein doo phantom test auto

tc: test-clj
ts: test-cljs
