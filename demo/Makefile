.PHONY: build
build:
	stack build --docker

.PHONY: clean
clean:
	stack clean --docker

.PHONY: simple-function
simple-function: build
	./function.sh simple

.PHONY: integration-test
integration-test: simple-function
	./sam/test.sh
