# The standard idiom: an explicit prerequisite list with no commands, and an
# inference rule that supplies them. The inferred source has to join the
# staleness comparison, or a changed `.c` never recompiles (audit #83).
inf_prereq.o: inf_prereq.h

.c.o:
	@echo "COMPILE $<"
	@touch $@
