%.o: %.c
	@echo PATCOMPILE; touch $@
.PHONY: all
all: utd_pat.o
	@echo ALL
