
BUILD_FILES=main.hi main.o main

build: 
	ghc --make -dynamic main.hs

clean: 
	rm $(BUILD_FILES)
