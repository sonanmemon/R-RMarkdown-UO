library(tidyverse)

library(ggplot2)

library(rvest)

library(lubridate)

library(janitor)

library(data.table)

library(hrbrthemes)


library(httr)

library(jsonlite)

library(fredr) 
               

library(listviewer) 

library(usethis)

library(fredr)






#API endpoint:



# Application 1: NY City Tree: No API Request Needed; Atypical.

nyc_trees = 
  fromJSON("https://data.cityofnewyork.us/resource/uvpi-gqnh.json") %>%
  as_tibble()








nyc_trees %>% 
  select(longitude, latitude, stump_diam, spc_common, spc_latin, tree_id) %>% 
  mutate_at(vars(longitude:stump_diam), as.numeric) %>% 
  ggplot(aes(x=longitude, y=latitude, size=stump_diam)) + 
  geom_point(alpha=0.5) +
  scale_size_continuous(name = "Stump diameter") +
  labs(
    x = "Longitude", y = "Latitude",
    title = "Sample of New York City trees",
    caption = "Source: NYC Open Data"
  )





# Application 2: FRED Data.

# https://api.stlouisfed.org/fred/series/observations?series_id=GNPCA&api_key=0b51a58cb47f65ee8544493548c656b4&file_type=json

endpoint = "series/observations"

params = list(
  api_key= Sys.getenv("FRED_API_KEY"), ## Change to your own FRED API Key
  file_type="json", 
  series_id="GNPCA"
)


fred = 
  httr::GET(
    url = "https://api.stlouisfed.org/", ## Base URL
    path = paste0("fred/", endpoint),    ## The API endpoint
    query = params                       ## Our parameter list
  )


fred = fred %>% 
  httr::content("text") %>% ## Extract the reponse content (i.e. text)
  jsonlite::fromJSON()      ## Convert from JSON to R object

## What type of object did we get?
typeof(fred)



View(fred)




fredobs =
  fred %>% 
  purrr::pluck("observations") %>% ## Extract the "$observations" list element
  # .$observations %>% ## I could also have used this
  # magrittr::extract("observations") %>% ## Or this
  as_tibble() ## Just for nice formatting

fredobs


fredobs =
  fredobs %>%
  mutate(across(realtime_start:date, ymd)) %>%
  mutate(GNP = as.numeric(value)) 


fredobs %>%
  ggplot(aes(date, GNP)) +
  geom_line() +
  scale_y_continuous(labels = scales::comma) +
  labs(
    x="Date", y="USD Billions (2012 Base Year)",
    title="US Real Gross National Product", caption="Source: FRED"
  )


View(fredobs)

library(fredr)

# Application 3: 

fredr(
  series_id = "UNRATE",
  observation_start = as.Date("1990-01-01"),
  observation_end = as.Date("2000-01-01")
)



# Applicayion: Rugby Data. Hard to extract API end point:


endpoint = "https://api.wr-rims-prod.pulselive.com/rugby/v3/rankings/mru?"


rugby = fromJSON(endpoint)

str(rugby)




listviewer::jsonedit(rugby, mode = "view")

head(rugby$entries$team)


rankings =
  bind_cols(
    rugby$entries$team,
    rugby$entries %>% select(pts:previousPos)
  ) %>%
  clean_names() %>%
  select(-c(id, alt_id, annotations)) %>% ## These columns aren't adding much of interest
  select(pos, pts, everything()) %>% ## Reorder remaining columns
  as_tibble() ## "Enhanced" tidyverse version of a data frame

rankings


## We'll look at rankings around Jan 1st each year. I'll use 2004 as an
## arbitrary start year and then proceed until the present year.

start_date = ymd("2004-01-01")

end_date = floor_date(today(), unit="years")

dates = seq(start_date, end_date, by="years")

## Get the nearest Monday to Jan 1st to coincide with rankings release dates.

dates = floor_date(dates, "week", week_start = getOption("lubridate.week.start", 1))

dates





## First remove our existing variables. This is not really necessary, since R is smart enough
## to distinguish named objects in functions from named objects in our global environment.
## But I want to emphasise that we're creating new data here and avoid any confusion.

rm(rugby, rankings, endpoint)

## Now, create the function. I'll call it "rugby_scrape".

rugby_scrape = 
  function(x) {
    endpoint = paste0("https://api.wr-rims-prod.pulselive.com/rugby/v3/rankings/mru?label=", x)
    rugby = fromJSON(endpoint)
    rankings =
      bind_cols(
        rugby$entries$team,
        rugby$entries %>% select(matches:previousPos)
      ) %>%
      clean_names() %>%
      mutate(date = x) %>% ## New column to keep track of the date 
      select(-c(id, alt_id, annotations)) %>% ## These columns aren't adding much of interest
      select(date, pos, pts, everything()) %>% ## Reorder remaining columns
      as_tibble() ## "Enhanced" tidyverse version of a data frame
    Sys.sleep(3) ## Be nice!
    return(rankings)
  }




rankings_history =
  lapply(dates, rugby_scrape) %>% ## Run the iteration
  bind_rows() ## Bind the resulting list of data frames into a single data frame

rankings_history






teams = c("NZL", "RSA", "ENG", "JPN")
team_cols = c("NZL"="black", "RSA"="#4DAF4A", "ENG"="#377EB8", "JPN" = "red")

rankings_history %>%
  ggplot(aes(x=date, y=pts, group=abbreviation)) +
  geom_line(col = "grey") + 
  geom_line(
    data = rankings_history %>% filter(abbreviation %in% teams), 
    aes(col=fct_reorder2(abbreviation, date, pts)),
    lwd = 1
  ) +
  scale_color_manual(values = team_cols) +
  labs(
    x = "Date", y = "Points", 
    title = "International rugby rankings", caption = "Source: World Rugby"
  ) +
  theme(legend.title = element_blank())



























































