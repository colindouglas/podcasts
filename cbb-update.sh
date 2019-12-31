TODAY="date +%Y-W%W"

Rscript cbb-scrape-earwolf.R

Rscript cbb-treatment.R

Rscript -e "library(rmarkdown); render(\"README.Rmd\")"

git add data/cbb_earwolf_scrape.csv images/cbb-bestof-plot.png README.md

git commit -m "Automated update for `$TODAY`"

git push
