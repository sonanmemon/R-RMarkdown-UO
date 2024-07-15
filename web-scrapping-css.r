
library(tidyverse)

library(ggplot2)

library(rvest)

library(lubridate)

library(janitor)

library(data.table)

library(hrbrthemes)


m100 = read_html("https://en.wikipedia.org/wiki/Men%27s_100_metres_world_record_progression")

m100




pre_iaaf = 
  m100 %>%
  html_element("div+ .wikitable :nth-child(1)") %>% ## select table element
  html_table()                                      ## convert to data frame

pre_iaaf


pre_iaaf =
  pre_iaaf %>%
  clean_names() %>% ## fix the column names
  mutate(date = mdy(date)) ## convert string to date format

pre_iaaf





#mw-content-text > div.mw-content-ltr.mw-parser-output > table:nth-child(11)

iaaf_76 =
  m100 %>%
  html_element("#mw-content-text > div > table:nth-child(17)") %>%
  html_table()



iaaf_76 =
  iaaf_76 %>%
  clean_names() %>%
  mutate(date = mdy(date)) ## convert string to date format

iaaf_76

iaaf =
  m100 %>%
  html_element("#mw-content-text > div > table:nth-child(23)") %>%
  html_table() %>%
  clean_names() %>%
  mutate(date = mdy(date))

iaaf
  
# stacking three different data subsets on top of each other using rbind:

wr100 =
  rbind(
    pre_iaaf %>% select(time, athlete, nationality, date) %>% 
      mutate(era = "Pre-IAAF"),
    iaaf_76 %>% select(time, athlete, nationality, date) %>% mutate(era = "Pre-automatic"),
    iaaf %>% select(time, athlete, nationality, date) %>% mutate(era = "Modern")
  )



wr100 %>%
  ggplot(aes(x=date, y=time, col=fct_reorder2(era, date, time))) +
  geom_point(alpha = 0.7) +
  labs(
    title = "Men's 100m world record progression",
    x = "Date", y = "Time",
    caption = "Source: Wikipedia"
  ) +
  theme(legend.title = element_blank()) ## Switch off legend title


## 


base_url = "https://www.imdb.com/chart/tvmeter/"

imdb_tv <- read_html(base_url)

View(imdb_tv)


Name  <-
  imdb_tv %>% 
  html_nodes(".ipc-title.ipc-title--base.ipc-title--title.ipc-title-link-no-icon.ipc-title--on-textPrimary.sc-b189961a-9.iALATN.cli-title") %>%
  html_text()

View(Name)


     

  
  TV_Series <-
  imdb_tv %>% 
  html_nodes(".sc-b189961a-3.hidKPx.cli-title-type-data") %>%
  html_text()
  
  View(TV_Series)
  

year <-
  imdb_tv %>% 
  html_nodes(".sc-b189961a-8.kLaxqf.cli-title-metadata-item") %>%
  html_text()

View(year)

##

base_url = "https://eugene.craigslist.org/search/sss?query=speakers&sort=rel&srchType=T"

craiglist = read_html(base_url)

View(craiglist)



price  <-
  craiglist %>% 
  html_nodes(".price") %>%
  html_text

View(price)





speakers <-
  craiglist %>%
  html_elements(".title, .price")


speakers <- html_text(speakers) ## parse as text

head(speakers, 20) ## show the first 20 entries




speakers_merge <- as.data.frame(t(matrix(speakers, nrow=2)))


View(speakers_merge)

speakers_merge$location = "Eugene, OR"

speakers_merge$time = "June/July 2024"



names(speakers_merge) = c('Speaker-Name', 'Price', 'Location', 'Month')






#ggplot(speakers_merge, aes(Price, Location)) +
 # labs(title = 'Speakers for sale near Eugene, OR',
  #     caption = 'Source: Craigslist',
  #     x = 'Price', y = 'L') +
  #theme_modern_rc()


























