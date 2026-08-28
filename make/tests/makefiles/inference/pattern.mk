%.o: %.c
	@echo "PATTERN target=$@ input=$< stem=$*"
all: pat_probe.o
	@echo done
