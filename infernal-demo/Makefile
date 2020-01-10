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

.PHONY: invoke-ok
invoke-ok: function
	cd ../sam-demo && sam local invoke --event events/ok-request.json

.PHONY: invoke-throw
invoke-throw: function
	cd ../sam-demo && sam local invoke --event events/throw-request.json

.PHONY: invoke-bad
invoke-bad: function
	cd ../sam-demo && sam local invoke --event events/bad-request.json
