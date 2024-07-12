


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

#conditional definition:

DT2 = data.table(a = -2:2, b = LETTERS[1:5])

DT2[a < 0, b := NA][]

# creating multiple columns simultaneously:

DT[, ':=' (y = 3:4, y_name = c("three", "four"))]

DT

# appending, inter-connected, two variable creations:

DT[, z := 5:6][, z_sq :=  z^2][]

# remove column y_name
DT[, y_name := NULL]
DT

#subset data by row and columns:

starwars_dt[1:2, c(1:3, 10)]

#or this:. .() is same as list(x, y) or c("var1", "var2", "var3").

starwars_dt[1:7, .(name, height, mass, homeworld)]

# Conditional Negation:

starwars_dt[, !c("name", "height")]



# Rename Columns Permanently:

# setnames(starwars_dt, old = c("name", "homeworld"), new = c("alias", "crib"))[]

# Temporary Renaming Columns:

starwars_dt[1:2, .(alias = name, crib = homeworld)]

# Aggregating Manipulations:

starwars_dt[, mean(height, na.rm=T)]


# creating variable rather than reporting:

starwars_dt[, mean_height := mean(height, na.rm=T)]


#count number of observations:

starwars_dt[, .N]


# group vise manipulations:


starwars_dt[, mean(mass, na.rm=T), by = height>190]


starwars_dt[, mean(height, na.rm=T), by = species]


# reports as species_height variable name:

starwars_dt[, .(species_height = mean(height, na.rm=T)), by = species]

# Aggregations By Multiple Variables:

starwars_dt[, .(mean_height = mean(height, na.rm=T)), by = .(species, homeworld)]

# Summarize multiple variables:

starwars_dt[,.(mean(height, na.rm=T), mean(mass, na.rm=T), mean(birth_year, na.rm=T)),
by = species]


# Efficient Subsetting With .SD:

starwars_dt[,
            lapply(.SD, mean, na.rm=T),
            .SDcols = c("height", "mass", "birth_year"),
            by = species] %>%
head(2) 

# Reports means of all variables since there are no NAs.

DT[, lapply(.SD, mean)]


# Setting keys:


setkey(DT, x)

#or 

#DT = as.data.table(d, key = "x")

# key can be set by two variables as well:

setkey(DT, x, y)




merge(DT1, DT2, by = "id")

library(nycflights13)

flights_dt = as.data.table(flights, key = "tailnum")

planes_dt = as.data.table(planes,  key = "tailnum")


merge(
  flights_dt,
  planes_dt,
  all.x = TRUE,
  by = "tailnum")

# to avoid confusion due to common "year" variable in both data sets.

merge(
  flights_dt,
  setnames(planes_dt, old = "year", new = "year_built"),
  all.x = TRUE,
  by = "tailnum")


#Reshaping:


stocks = data.table(time = as.Date('2009-01-01') + 0:1,
                    X = rnorm(2, 0, 1),
                    Y = rnorm(2, 0, 2),
                    Z = rnorm(2, 0, 4))

# From Wide to Long. melt function for data.table:

# melt(stocks, id.vars = "time")

stocks_long = melt(stocks, id.vars ="time",
                   variable.name = "stock", value.name = "price")
stocks_long

# using tidy fast and wide to long:

library(tidyfast)


stocks %>%
  dt_pivot_longer(X:Z, names_to = "stock", values_from = "price")

# For long to wide:

dcast(stocks_long,
      time ~ stock,
      value.var = "price")

# using tidy fast for long to wide:


stocks_long %>%
  dt_pivot_wider(names_from=stock,
                 values_from=price)






































































































