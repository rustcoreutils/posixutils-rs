.SUFFIXES: .a .b

.a.b:
	@echo "WRONG: inference rule ran as default"

real_target:
	@echo "CORRECT: real target ran"
