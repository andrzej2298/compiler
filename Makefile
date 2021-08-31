all: compiler runtime.o

compiler: src/*.hs
	ghc src/Compiler.hs \
		-Wall -Wincomplete-patterns\
		-i grammar/AbsLatte \
		-i grammar/LexLatte \
		-i grammar/ParLatte \
		-i grammar/ErrM \
		-i src/CommonDeclarations.hs \
		-i src/static_analyser/StaticAnalysisCommonDeclarations.hs \
		-i src/static_analyser/ExpressionAnalyser.hs \
		-i src/static_analyser/StatementAnalyser.hs \
		-i src/static_analyser/StaticAnalyser.hs \
		-i src/code_generator/CodeGenerationCommonDeclarations.hs \
		-i src/code_generator/StatementGenerator.hs \
		-i src/code_generator/ExpressionGenerator.hs \
		-i src/code_generator/CodeGenerator.hs \
		-o src/Compiler

runtime.o: ./lib/runtime.c
	clang -Wall -Wextra -Werror -emit-llvm -S ./lib/runtime.c -c -o ./lib/runtime.ll
	llvm-as ./lib/runtime.ll -o ./lib/runtime.bc

tar:
	mkdir -p latte
	mkdir latte/src
	mkdir latte/lib
	mkdir latte/grammar
	cp -r src/* latte/src
	cp -r lib/* latte/lib
	cp grammar/* latte/grammar
	cp Makefile latte/Makefile
	cp latc_llvm latte/latc_llvm
	cp latc latte/latc
	cp README.md latte/README.md
	tar -czvf latte.tar.gz latte

.PHONY: clean
clean:
	- rm src/Compiler
	- rm src/*.o
	- rm src/*.hi
	- rm src/code_generator/*.o
	- rm src/code_generator/*.hi
	- rm src/static_analyser/*.o
	- rm src/static_analyser/*.hi
	- rm lib/*.ll
	- rm lib/*.bc
	- rm -r latte
	- rm latte.tar.gz
