run:
	@./run

build:
	@./build

clean:
	@-rm marad *.core 2>/dev/null

.PHONY: build clean run
