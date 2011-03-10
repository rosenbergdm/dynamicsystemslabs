#!/bin/zsh
# Dynamic Systems Lab Generic Makefile

RCMD = $(shell which R64)
TEXCMD = $(shell which lualatex)
SWEAVECMD = $(RCMD) CMD Sweave
STANGLECMD = $(RCMD) CMD Stangle
OPENCMD = open
# RNW := $(shell egrep -l '^[^%]*\\begin\{document\}' *.Rnw)
# TEX = $(RNW:%.Rnw=%.tex)
# PDF = $(RNW:%.Rnw=%.pdf)
PDF = exercise.pdf guide.pdf solution.pdf
TEX = exercise.tex guide.tex solution.tex
RNW = exercise.Rnw guide.Rnw solution.Rnw
R = exercise.R guide.R solution.R

all: guide exercise source solution

.PHONY:  all clean guide exercise source

solution: $(PDF)
	$(OPENCMD) solution.pdf

guide: $(PDF)
	$(OPENCMD) guide.pdf

exercise: $(PDF)
	$(OPENCMD) exercise.pdf

source: $(R)

%.R: %.Rnw
	$(STANGLECMD) $< -o $@

%.tex: %.Rnw
	$(SWEAVECMD) $< -o $@

%.pdf: %.tex
	$(TEXCMD) $< -o $@

clean:
	rm -Rf *.pdf; rm -Rf *.aux; rm -Rf *.log
