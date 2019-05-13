build_site:
	Rscript -e "pkgdown::build_site()"
	sed -i 's/MartÃ­nez-GarcÃ­a/Martínez-García/g' docs/authors.html

cran_check:
	Rscript -e "devtools::check()"
	Rscript -e "devtools::check_win_devel(quiet = TRUE)"
	Rscript -e "rhub::check_for_cran()"
	Rscript -e "rhub::check(platform = 'ubuntu-rchk')"
	Rscript -e "rhub::check_with_sanitizers()"

check_resolve:
	git diff -S "<<<<<<< HEAD" -S "=======" -S ">>>>>>> $(git name-rev --name-only MERGE_HEAD)" HEAD
