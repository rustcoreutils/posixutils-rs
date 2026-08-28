# Two inference rules that derive each other's source. The edge an inference
# rule contributes was invisible to the cycle check, so the one thread blocked
# on a target it was itself in the middle of building (audit #80).
.c.o:
	@echo CO
.o.c:
	@echo OC
