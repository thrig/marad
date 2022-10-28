build:
	@./compile

clean:
	@-rm marad *.core 2>/dev/null

run:
	@./run

.PHONY: build clean run
