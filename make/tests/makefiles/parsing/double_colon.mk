# GNU's double-colon rule. POSIX has no such construct and we do not implement
# it; taking the first `:` made the second one a prerequisite named `:`
# (audit #86).
all:: dc_dep
	@echo DC
