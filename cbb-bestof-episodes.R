### Episodes in the best of, in order, #1 at the start of the vector and descending from there

cbbBestOf <- data_frame(
  year = 2009, 
  rank = 1:10, 
  number = c("B1",29,18,17,12,26,28,15,16,14)) %>%
  
  rbind(data_frame(
    year = 2010,
    rank = 5,
    number = c(34,35,51,59,"BO2009",73,43,76.5,69,51))) %>%
  
  rbind(data_frame(
    year = 2011,
    rank = 1:10,
    number = c(120,99,94,103,106.5,85,131,126,96,109))) %>%
  
  rbind(data_frame(
    year = 2012,
    rank = 1:10,
    number = c(150,166,176,148,167,154,162,156,180,174))) %>%
  
  rbind(data_frame(
    year = 2013,
    rank = 1:15,
    number = c(215,238,200,245,219,199,236,229,218,243,241,222,211,204,203))) %>%
  
  rbind(data_frame(
    year = 2014,
    rank = 1:15,
    number = c(300,274,289,286,304,285,312,283,263,272,309,266,265,301,310))) %>%
  
  rbind(data_frame(
    year = 2015,
    rank = 1:14,
    number = c(356,336,355,338,342,349,378,377,354,365,335,351,327,329))) %>%
  
  rbind(data_frame(
    year = 2016,
    rank = 1:15,
    number = c(406,416,391,419,429,446,390,425,402,423,401,400,452,456,393))) %>%
  
  rbind(data_frame(
    year = 2017,
    rank = 1:16,
    number = c(500,485,463,511,484,512,481,478,510,518,514,521,473,489,469,474))) 

write_csv(cbbBestOf, "data/cbb_bestof_episodes.csv")
