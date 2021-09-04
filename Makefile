# TODO don't use makefile?
.DUMMY: release
release:
	clojure -M:shadow-cljs release app

# need different target
# watch:
# 	clojure -M:shadow-cljs watch app
