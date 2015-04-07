TARG = haskell-to-hardware

PDFs = figures/applyLin-t21.pdf figures/applyLin-t22.pdf figures/applyLin-v23.pdf figures/applyLin-v34.pdf figures/applyLin-v42.pdf figures/applyLin-v45.pdf figures/bitonic-up-1.pdf figures/bitonic-up-2.pdf figures/bitonic-up-3.pdf figures/bitonic-up-4.pdf figures/composeLin-t222.pdf figures/composeLin-t232.pdf figures/composeLin-v234.pdf figures/crcSKpp-rt2-shallow-delay-with-depths.pdf figures/dotsp-pt1.pdf figures/dotsp-pt2.pdf figures/dotsp-pt3.pdf figures/dotsp-pt4.pdf figures/dotsp-t2t2.pdf figures/dotsp-v3t2.pdf figures/evalPoly-rt4.pdf figures/fibS.pdf figures/lsumsp-lt4.pdf figures/lsumsp-rt4.pdf figures/lsumsp-rt5.pdf figures/map-t3.pdf figures/map-t4.pdf figures/map-v6.pdf figures/powers-rt4-no-opt.pdf figures/powers-rt4.pdf figures/shiftR-iota-v3.pdf figures/sum-2.pdf figures/sum-4a.pdf figures/sum-4b.pdf figures/sum-p.pdf figures/sum-t4.pdf figures/sum-v6-0.pdf figures/sum-v6.pdf figures/sumSquare-p.pdf figures/sumSquare-t2.pdf figures/sumSquare-t3.pdf figures/sumSquare-t4.pdf figures/transpose-pt4.pdf figures/transpose-t4p.pdf

# figs: $(PDFs)

figures/%.pdf: dots/%.dot figures makefile
	dot -Tpdf $< -o $@

.PRECIOUS: %.tex %.pdf %.web

all: $(TARG).pdf

see: $(TARG).see

figures:
	mkdir figures

$(TARG).pdf: $(TARG).tex makefile $(PDFs)
	pdflatex $(TARG).tex

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
