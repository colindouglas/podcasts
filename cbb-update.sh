# This as YYYY-WW
TODAY="date +%Y-W%W"

# Working directory for running from cron
WKDIR=~/projects/podcasts
cd $WKDIR

Rscript cbb-scrape-earwolf.R

Rscript cbb-treatment.R

Rscript -e "Sys.setenv(RSTUDIO_PANDOC='/usr/lib/rstudio/bin/pandoc'); library(rmarkdown); render('README.Rmd')"

git add data/cbb_earwolf_scrape.csv images/cbb-bestof-plot.png README.md
git commit -m "Automated update for `$TODAY`"
git push
