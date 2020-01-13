.PHONY: build
build:
	stack build --docker

.PHONY: clean
clean:
	stack clean --docker

.PHONY: function
function: build
	rm -rf .build
	mkdir .build
	cp `stack --docker path --local-install-root`/bin/infernal-simple-demo .build/bootstrap
	cd .build && ls -lh bootstrap && zip function.zip bootstrap && rm bootstrap
	unzip -l .build/function.zip

.PHONY: integration-test
integration-test: function
	./sam/test.sh
