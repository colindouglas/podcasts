TODAY="date +%Y-w%W"

# Update the dataset from the Earwolf website
Rscript scrape-earwolf.R

# Re-render the Best Of analysis
Rscript -e "rmarkdown::render(\"README.Rmd\")"
git add data/earwolf_podcasts.csv data/earwolf_podcasts.Rda images/cbb-bestof-plot-1.png README.md
git commit -m "Automated update for `$TODAY`"
git push
