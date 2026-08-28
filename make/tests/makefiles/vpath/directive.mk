vpath %.c vpath_dir_probe
.SUFFIXES:
.SUFFIXES: .c .o
.c.o:
	@echo "IN=$<"
all: vpd_probe.o
	@true
