all:
	$(info Builing ocaml binary...)
	./build.sh
	$(info Building test binary...)
	./build-test.sh
	$(info Running tests...)
	./run-test-sequential.sh
	
