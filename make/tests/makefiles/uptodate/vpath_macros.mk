# `VPATH` supplies the prerequisite, so every macro naming it must name where
# it was found -- not just `$<` (audit #85).
VPATH = vpm_probe_src

all: vpm_a.txt
	@echo "LT=[$<] CARET=[$^] PLUS=[$+] Q=[$?]"
