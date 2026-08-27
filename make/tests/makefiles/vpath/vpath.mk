VPATH = vpath_src
.SUFFIXES:
.SUFFIXES: .c .o
.c.o:
	@echo "IN=$<"
all: vp_probe.o
	@true
