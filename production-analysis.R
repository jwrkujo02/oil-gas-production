# start by loading libraries needed for the project. this creates transparency to
# refer back to.

library(readr) # built in reading packages aren't perfect.
library(dplyr)
library(lubridate)

# remembering the difference between interpreted and compiled languages:
# the R Console can receive information in its terminal and immediately spit out
# in other non interpreted languages, it requires writing script, saving file, and
# utilizing a compiler such as GCC. compiler takes the commands and changes them
# from computer language 

# in packages section, we can view available packages and distinguish between currently loaded
# packages. in each session, packages will stay loaded until you close them. overall, it is
# good to remember which libraries are open at a time

prod21 <- readr::read_csv() #clarifies the read_csv from the readr library specifically

#loads file in box directly from the web
prod21 <- read_csv("https://duq.box.com/v/OilGasProduction2021") 

# this function retrieves the directory you are currently working out of. 
getwd()

# to set a path from the computer itself, use setwd() and enter desired location
setwd("C:\\Users\\parkj\\Documents\\R\\oil-gas-production")

getwd()

# time to actually load the data. 
prod21 <- read_csv("OilGasProduction2021.csv")
prod22 <- read_csv("OilGasProduction2022.csv")
prod23 <- read_csv("OilGasProduction2023.csv")

# function useful for combining data into one data frame (if using multiple sets)
prod <- rbind(prod21, prod22, prod23)

# rm() function removes the variables specified.
rm(prod21, prod22, prod23)

# telling computer to take data from prod variable, but specify only ones under Greene county.
greene <- prod %>% 
  filter(COUNTY == "Greene") %>%
  group_by(PERIOD_ID) %>% 
  summarize(sum(GAS_QUANTITY, na.rm = TRUE))

