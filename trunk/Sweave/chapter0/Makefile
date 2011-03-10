#!/bin/sh
# Dynamic Systems Lab Generic Makefile

RCMD = $(shell which R64)
TEXCMD = $(shell which lualatex)
SWEAVECMD = $(RCMD) CMD Sweave
OPENCMD = open
# RNW := $(shell egrep -l '^[^%]*\\begin\{document\}' *.Rnw)
# TEX = $(RNW:%.Rnw=%.tex)
# PDF = $(RNW:%.Rnw=%.pdf)
PDF = lab_exercise.pdf guide_chapter0.pdf solutions.pdf
TEX = lab_exercise.tex guide_chapter0.tex solutions.tex
RNW = lab_exercise.Rnw guide_chapter0.Rnw solutions.Rnw

all: guide exercise solutions

.PHONY:  all clean guide exercise solutions

solutions: $(PDF)
	$(OPENCMD) solutions.pdf

guide: $(PDF)
	$(OPENCMD) guide_chapter0.pdf

exercise: $(PDF)
	$(OPENCMD) lab_exercise.pdf

%.tex: %.Rnw
	$(SWEAVECMD) $< -o $@

%.pdf: %.tex
	$(TEXCMD) $< -o $@

clean:
	rm -Rf *.pdf; rm -Rf *.aux; rm -Rf *.log
