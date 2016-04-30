help:
	@cat Makefile

deploy:
	lein deploy clojars

test-clj:
	lein test

test-cljs:
	lein cljsbuild test once

tc: test-clj
ts: test-cljs
