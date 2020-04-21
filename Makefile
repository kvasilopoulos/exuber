build_site:
	Rscript -e "pkgdown::build_site()"
	sed -i 's/MartÃ­nez-GarcÃ­a/Martínez-García/g' docs/authors.html

fix_author:
	sed -i 's/MartÃ­nez-GarcÃ­a/Martínez-García/g' docs/authors.html

cran_check:
	Rscript -e "devtools::check_win_devel(quiet = TRUE)"
	Rscript -e "devtools::check_rhub(interactive = FALSE)"

check_resolve:
	git diff -S "<<<<<<< HEAD" -S "=======" -S ">>>>>>> $(git name-rev --name-only MERGE_HEAD)" HEAD
