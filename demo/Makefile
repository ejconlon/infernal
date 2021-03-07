.PHONY: build
build:
	stack build --docker

.PHONY: clean
clean:
	stack clean --docker

.PHONY: integration-test-simple
integration-test-simple: clean build
	./build-function.sh simple
	./sam/test.sh simple InfernalSimpleDemoFunction

.PHONY: integration-test-wai
integration-test-wai: clean build
	./build-function.sh wai
	./sam/test.sh wai InfernalWaiDemoFunction

.PHONY: integration-test
integration-test: integration-test-simple integration-test-wai
