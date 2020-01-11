integration-test:
	cd infernal-demo && make function
	./sam-demo/test.sh
