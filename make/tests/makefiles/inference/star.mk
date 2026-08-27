.SUFFIXES:
.SUFFIXES: .c .o
.c.o:
	@echo "STAR=[$*] AT=[$@] LT=[$<]"
all: star_probe.o
	@true
