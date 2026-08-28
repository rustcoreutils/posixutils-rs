all: fn_probe_src/a.o fn_probe_src/b.o
	@echo "notdir=[$(notdir $^)] dir=[$(dir $<)] base=[$(basename $^)]"
