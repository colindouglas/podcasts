TODAY="date +%Y-w%W"

Rscript scrape-earwolf.R

Rscript -e "rmarkdown::render(\"README.Rmd\")"

Rscript deploy-shinyapp.R

git add data/earwolf_podcasts.csv data/earwolf_podcasts.Rda images/cbb-bestof-plot-1.png README.md

git commit -m "Automated update for `$TODAY`"

git push
