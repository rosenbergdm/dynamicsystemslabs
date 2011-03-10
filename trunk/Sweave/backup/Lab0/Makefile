# Lab0 Sweave Makefile
#
RCmd = $(shell which R)
SweaveCmd = $(RCmd) CMD Sweave
StangeCmd = $(RCmd) CMD Stangle
XetexCmd = $(shell which xelatex)
Rnwfiles = $(shell ls *.Rnw)


all: typeset

.PHONY: clean typeset tangle sweave

Lab0.R: Lab0.Rnw
	$(StangleCmd) Lab0.Rnw

tangle: Lab0.R

sweave: Lab0.tex

typeset: Lab0.pdf

Lab0.pdf: Lab0.tex
	$(XetexCmd) Lab0.tex
	$(XetexCmd) Lab0.tex


Lab0.tex: Lab0.Rnw
	$(SweaveCmd) Lab0.Rnw 

clean:	
	rm -Rf *.{pdf,aux,log,tex,out}
	rm -Rf cache/*
