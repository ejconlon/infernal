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
	cp `stack --docker path --local-install-root`/bin/infernal-demo .build/bootstrap
	cd .build && zip function.zip bootstrap && rm bootstrap
	unzip -l .build/function.zip
