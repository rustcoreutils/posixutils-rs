.PHONY: phony_probe_a
.PHONY: phony_probe_b
phony_probe_a:
	@echo PHONY-phony_probe_a
phony_probe_b:
	@echo PHONY-phony_probe_b
all: phony_probe_a phony_probe_b
	@echo x
