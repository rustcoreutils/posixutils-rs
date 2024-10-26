.SUFFIXES: .txt .out  

.txt.out:  
	@echo "Converting testfile.txt to testfile.out" 
	@cp testfile.txt testfile.out


testfile.out: testfile.txt


