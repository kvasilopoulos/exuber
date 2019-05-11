build_site:
	Rscript -e "pkgdown::build_site()"
	sed -i 's/MartÃ­nez-GarcÃ­a/Martínez-García/g' docs/authors.html
