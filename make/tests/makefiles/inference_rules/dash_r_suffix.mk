.SUFFIXES: .txt .out

.txt.out:
	@echo "Converting $< to $@"
	@cp $< $@

dash_r_test.out: dash_r_test.txt
