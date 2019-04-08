
readme:
	Rscript -e "rmarkdown::render('README.Rmd')"

build:
	Rscript -e "devtools::build()"

check:
	Rscript -e "devtools::check()"

install:
	Rscript -e "devtools::install(dependencies = FALSE)"

winbuild:
	Rscript -e "devtools::check_win_devel(quiet = TRUE)"
	Rscript -e "devtools::check_win_release(quiet = TRUE)"

pkgdown:
	Rscript -e "pkgdown::build_site()"

fix_authors:
	sed -i 's/MartÃ�nez-GarcÃ�a/Martínez-García/g' docs/authors.html
