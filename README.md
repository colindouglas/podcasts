
# Overview

Comedy Bang Bang is an alt-comedy improv podcast published at least
weekly since 2009. Each episode features one or more guests, who may be
playing themselves or an improvised character. Since its inception,
there have been 675 mainline episodes with 481 unique guests.

At the end of the year, listeners vote on the best episodes of the year
(Thanksgiving-to-Thanksgiving), which are compiled in year-end Best Of
episodes that are published over the Christmas-New Year break. Paul F.
Tompkins, the most frequent CBB guest, typically cohosts these shows
with the show’s host, Scott Aukerman.

In 2017, there was a lot of Scott-and-Paul talk in the Best Ofs about
the frequency in which Paul appears in Best Of’d episodes, I decided to
test whether this was because Paul makes episodes good, or it he’s just
in a lot of episodes. To do this, I used the statistical computing
package R, alongside the tidyverse and rvest packages.

I scraped the data directly from the Earwolf website using the
`cbb-scrape-earwolf.R`, and then created the image via
`cbb-treatment.R`.

![Top Comedy Bang Bang Guests by
Episode](https://github.com/colindouglas/podcasts/blob/master/images/cbb-bestof-plot.png)
