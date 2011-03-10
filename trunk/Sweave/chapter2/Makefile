#!/bin/zsh
# Dynamic Systems Lab Generic Makefile

RCMD = $(shell which R64)
TEXCMD = $(shell which lualatex)
SWEAVECMD = $(RCMD) CMD Sweave
STANGLECMD = $(RCMD) CMD Stangle
OPENCMD = open
#PGFSWEAVE = pgfsweave --pgfsweave-only
PGFSWEAVE = $(RCMD) CMD pgfsweave --pgfsweave-only

# RNW := $(shell egrep -l '^[^%]*\\begin\{document\}' *.Rnw)
# TEX = $(RNW:%.Rnw=%.tex)
# PDF = $(RNW:%.Rnw=%.pdf)
PDF = exercise.pdf guide.pdf solution.pdf week2_lecture.pdf
TEX = exercise.tex guide.tex solution.tex week2_lecture.tex
RNW = exercise.Rnw guide.Rnw solution.Rnw week2_lecture.Rnw
R = exercise.R guide.R solution.R week2_lecture.R

all: guide exercise source solution lecture

.PHONY:  all clean guide exercise source lecture

solution: $(PDF)
	$(OPENCMD) solution.pdf

guide: $(PDF)
	$(OPENCMD) guide.pdf

exercise: $(PDF)
	$(OPENCMD) exercise.pdf

source: $(R)

lecture: $(PDF)
	$(OPENCMD) week2_lecture.pdf

%.R: %.Rnw
	$(STANGLECMD) $< -o $@

%.tex: %.Rnw
	$(PGFSWEAVE) $< -o $@

%.pdf: %.tex
	$(TEXCMD) $< -o $@

clean:
	rm -Rf *.pdf; rm -Rf *.aux; rm -Rf *.log
