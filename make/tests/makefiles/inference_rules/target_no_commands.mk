.SUFFIXES: .src .dst

.src.dst:
	@echo "Inference applied: $< -> $@"
	@cp $< $@

inference_test.dst: inference_test.src
