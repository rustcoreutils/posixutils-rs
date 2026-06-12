all:
	@echo result=ok
	@test x = x && echo testeq_ok
	@echo ./configure --prefix=/usr
