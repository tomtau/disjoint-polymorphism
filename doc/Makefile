MAIN = paper

SCRIPT = script

all: $(MAIN).pdf

$(MAIN).pdf: $(wildcard ../examples/*.txt) $(wildcard sections/*.ott) $(wildcard sections/*.tex) $(MAIN).tex paper.bib
	make -C sections
	@ruby $(SCRIPT).rb
	@latexmk -pdf $(@:.pdf=.tex)

appendix:
	@latexmk -pdf appendix.tex

submit: $(MAIN).pdf appendix
	xelatex annotated_reviews.tex
	rm -Rf impl && mkdir -p impl
	rsync --relative --files-from submit.txt ../ impl
	tar zcf bundle.tar.gz README.md appendix.pdf impl
	mkdir -p upload
	mv bundle.tar.gz $(MAIN).pdf annotated_reviews.pdf upload/

clean:
	@rm -f $(MAIN).pdf $(MAIN).bbl *.aux *.fls *.out *.synctex.gz *.log *.fdb_latexmk *.blg bundle.tar.gz
	@rm -f notes/memo.tex appendix.pdf
	@rm -Rf auto/ sections/auto/ impl upload/

.PHONY: all clean submit appendix
