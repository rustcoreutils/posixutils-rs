all: a b
	@echo SHOULD-NOT-RUN
a:
	@false
b:
	@echo B-RAN
