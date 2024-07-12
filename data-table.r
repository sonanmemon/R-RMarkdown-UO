


library(dplyr)


starwars %>%
  filter(species == "Human") %>%
  group_by(homeworld) %>%
  summarise(mean_height=mean(height))

library(data.table) 

starwars_dt = as.data.table(starwars)
starwars_dt[species == "Human", mean(height), by=homeworld]

data.table(x = 1:10)



data(starwars, package = "dplyr")

starwars %>%
  filter(species == "Human") %>%
  group_by(gender) %>%
  summarise(mean(height, na.rm=T))

# vs data.table:


starwars_dt = as.data.table(starwars)

starwars_dt[species == "Human",
  mean(height, na.rm=T),
  by = gender]



starwars_dt[height>190 & species == "Human"]

starwars_dt[order(birth_year)] # temporary


setorder(starwars_dt, birth_year, na.last = TRUE)

starwars_dt[1:5, name:birth_year] # Only print subset to stay on the slide


DT = data.table(x = 1:2)

DT[, x_sq := x^2][] # Adding [] prints the result.











































