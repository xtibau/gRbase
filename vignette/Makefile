whoami=$(shell whoami)

all: 	
	make gRbase-graphs.pdf
	make gRbase-arrayops1.pdf
	make gRbase-arrayops2.pdf

clean:
	rm -f *.tex *.bbl *.blg *.aux *.out *.log *.toc *.Rnw~ 


setvars:
ifeq (${R_HOME},)
R_HOME=	$(shell R RHOME)
endif
RPROG=	$(R_HOME)/bin/R
RSCRIPT=$(R_HOME)/bin/Rscript

gRbase-graphs.pdf:
	$(RPROG) CMD Sweave --encoding=latin1 gRbase-graphs.Rnw
	$(RSCRIPT) --vanilla -e "tools::texi2dvi( 'gRbase-graphs.tex', pdf = TRUE, clean = FALSE )"
	$(RSCRIPT) --vanilla -e "tools::texi2dvi( 'gRbase-graphs.tex', pdf = TRUE, clean = FALSE )"
	rm -fr *.tex

gRbase-arrayops1.pdf:
	$(RPROG) CMD Sweave --encoding=latin1 gRbase-arrayops1.Rnw
	$(RSCRIPT) --vanilla -e "tools::texi2dvi( 'gRbase-arrayops1.tex', pdf = TRUE, clean = FALSE )"
	$(RSCRIPT) --vanilla -e "tools::texi2dvi( 'gRbase-arrayops1.tex', pdf = TRUE, clean = FALSE )"
	rm -fr *.tex

gRbase-arrayops2.pdf:
	$(RPROG) CMD Sweave --encoding=latin1 gRbase-arrayops2.Rnw
	$(RSCRIPT) --vanilla -e "tools::texi2dvi( 'gRbase-arrayops2.tex', pdf = TRUE, clean = FALSE )"
	$(RSCRIPT) --vanilla -e "tools::texi2dvi( 'gRbase-arrayops2.tex', pdf = TRUE, clean = FALSE )"
	rm -fr *.tex


