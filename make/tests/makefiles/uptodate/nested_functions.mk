# A function call nested inside another, around an automatic variable. Reading
# only to the first `)` truncated the outer call and left the real one in the
# command (audit #79).
all: nf_probe_src/x.c
	@echo "prefixed=[$(addprefix o/,$(notdir $^))]"
	@echo "stem=[$(basename $(notdir $<))]"
	@echo "deep=[$(addprefix o/,$(basename $(notdir $^)))]"
