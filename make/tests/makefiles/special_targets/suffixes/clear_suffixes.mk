.SUFFIXES: 

.txt.out:  
	@echo "Converting $< to $@"
	@cp copied.txt copied.out


copied.out: copied.txt

