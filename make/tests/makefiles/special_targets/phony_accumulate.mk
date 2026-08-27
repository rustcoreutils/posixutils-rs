.PHONY: a
.PHONY: b
a:
	@echo PHONY-a
b:
	@echo PHONY-b
all: a b
	@echo x
