TODAY="date +%Y-w%W"

Rscript scrape-earwolf.R

Rscript -e "knitr::knit(\"README.Rmd\")"

git add data/earwolf_podcasts.csv data/earwolf_podcasts.Rda images/cbb-bestof-plot-1.png README.md

git commit -m "Automated update for `$TODAY`"

git push
