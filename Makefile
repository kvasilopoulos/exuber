fix_author:
	sed -i 's/MartÃ­nez-GarcÃ­a/Martínez-García/g' docs/authors.html

build_reference:
	Rscript -e "devtools::document(roclets = c('rd', 'collate', 'namespace'))"
	Rscript -e "pkgdown::build_reference()"

build_site:
	Rscript -e "devtools::document(roclets = c('rd', 'collate', 'namespace'))"
	Rscript -e "pkgdown::build_site()"
	sed -i 's/MartÃ­nez-GarcÃ­a/Martínez-García/g' docs/authors.html

build_manual:
	Rscript -e "devtools::build_manual"

check:
	Rscript -e "devtools::check()"

cran_check:
	Rscript -e "devtools::check_win_devel(quiet = TRUE)"
	Rscript -e "devtools::check_rhub(interactive = FALSE)"

git_resolve:
	git diff -S "<<<<<<< HEAD" -S "=======" -S ">>>>>>> $(git name-rev --name-only MERGE_HEAD)" HEAD
