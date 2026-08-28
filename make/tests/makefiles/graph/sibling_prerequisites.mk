# `a b:` is two rules, not one shared rule. A later rule for `a` used to add
# its prerequisites to `b` as well (audit #84).
sib_a sib_b:
	@echo "T=$@ CARET=[$^]"

sib_a: sib_extra

sib_extra:
	@echo BUILT-EXTRA
