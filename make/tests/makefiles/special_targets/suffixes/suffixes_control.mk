# .txt and .out are not in the default .SUFFIXES list, so they have to be added
# before an empty .SUFFIXES: can be shown to take them away again. Without the
# clearing line this builds copied.out through .txt.out; with it, nothing does.
# clear_suffixes.mk is the same makefile with the clearing line restored.
.SUFFIXES: .txt .out

.txt.out:
	@echo "Converting $< to $@"

copied.out: copied.txt
