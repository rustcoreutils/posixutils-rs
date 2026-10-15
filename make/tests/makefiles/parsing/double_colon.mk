# GNU's double-colon rule. Taking the first `:` alone made the second one a
# prerequisite named `:` (audit #86).
all:: dc_dep
	@echo DC
dc_dep:
	@echo DEP
