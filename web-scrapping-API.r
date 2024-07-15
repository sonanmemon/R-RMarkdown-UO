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















































