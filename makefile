TARG = haskell-to-hardware

.PRECIOUS: %.tex %.pdf %.web

all: $(TARG).pdf

see: $(TARG).see

%.pdf: %.tex makefile
	pdflatex $*.tex

# --poly is default for lhs2TeX

%.tex: %.lhs macros.tex mine.fmt makefile
	lhs2TeX -o $*.tex $*.lhs

showpdf = open -a Skim.app

%.see: %.pdf
	${showpdf} $*.pdf

clean:
	rm $(TARG).{tex,pdf,aux,nav,snm,ptb}

web: web-token

web-token: $(TARG).pdf
#	scp $? conal@conal.net:/home/conal/web/talks
	scp $? conal@conal.net:/home/conal/web/misc
	touch $@
