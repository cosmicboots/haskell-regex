BUILD_FILES=main.hi main.o main

build: 
	ghc --make -dynamic main.hs

testbuild:
	mkdir -p tests/output

cleantests:
	rm -r tests/output

test1: testbuild build
	@./main "\[.*" "" tests/act_2_sc6.txt > tests/output/test1.txt
	@sed 's/\[.*//g' tests/act_2_sc6.txt > tests/output/solution1.txt
	diff tests/output/solution1.txt tests/output/test1.txt

test2: testbuild build
	@./main "FRIAR LAWRENCE" "Fat Man" tests/act_2_sc6.txt > tests/output/test2.txt
	@sed "s/FRIAR LAWRENCE/Fat Man/g" tests/act_2_sc6.txt > tests/output/solution2.txt
	diff tests/output/solution2.txt tests/output/test2.txt

test3: testbuild build
	@./main "A(men), a\1" "ARE YOU HAPPY NOW" tests/act_2_sc6.txt > tests/output/test3.txt
	@sed "s/A\(men\), a\1/ARE YOU HAPPY NOW/g" tests/act_2_sc6.txt > tests/output/solution3.txt
	diff tests/output/solution3.txt tests/output/test3.txt

test4: testbuild build
	@./main "JULIE(T)" "\0\1" tests/act_2_sc6.txt > tests/output/test4.txt
	@sed "s/JULIE\(T\)/\0\1/g" tests/act_2_sc6.txt > tests/output/solution4.txt
	diff tests/output/solution4.txt tests/output/test4.txt

test5: testbuild build
	@./main "{T|t}h[e-y]" "the" tests/act_2_sc6.txt > tests/output/test5.txt
	@sed "s/\(T\|t\)h[e-y]/the/g" tests/act_2_sc6.txt > tests/output/solution5.txt
	diff tests/output/solution5.txt tests/output/test5.txt

testall: test1 test2 test3 test4 test5

clean: cleantests
	rm $(BUILD_FILES)
