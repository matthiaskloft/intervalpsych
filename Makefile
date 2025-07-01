all: clean vignettes docs delete_claude

docs:
	( Rscript -e "pkgdown::build_site('..', install=FALSE)" )

articles: vignettes
	( Rscript -e "pkgdown::build_articles('..')" )

vignettes:
	( cd ../vignettes && Rscript ./rebuild.r )

clean:
	rm -rf html

delete_claude:
	rm -f ../docs/CLAUDE.html


