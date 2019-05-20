BUILD_DIR=build
X_OPTS=
LXC=latexmk -xelatex -shell-escape $(X_OPTS) -outdir=$(BUILD_DIR)


.PHONY: default
default: all;

build_dir:
	mkdir -p $(BUILD_DIR)

define compile_tex
	$(LXC) $(1).tex
	cp $(BUILD_DIR)/$(1).pdf .
endef

01: build_dir
	$(call compile_tex,01.intro-curso)

02: build_dir
	$(call compile_tex,02.organiz-comput)

03: build_dir
	$(call compile_tex,03.intro-C)

04: build_dir
	$(call compile_tex,04.ES-tipos)

05: build_dir
	$(call compile_tex,05.operacoes-rel-log)

06: build_dir
	$(call compile_tex,06.condicionais)

07: build_dir
	$(call compile_tex,07.repeticao)

08: build_dir
	$(call compile_tex,08.funcoes)

09: build_dir
	$(call compile_tex,09.recursao)

10: build_dir
	$(call compile_tex,10.vetores.matrizes)

11: build_dir
	$(call compile_tex,11.ponteiros.parte1)

12: build_dir
	$(call compile_tex,12.ponteiros.parte2)

13: build_dir
	$(call compile_tex,13.structs)

14: build_dir
	$(call compile_tex,14.arquivos)

15: build_dir
	$(call compile_tex,15.ordenacao)

16: build_dir
	$(call compile_tex,16.listas.ligadas)

all: 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16

clean:
	rm -fr build

full_clean: clean
	rm *.pdf
