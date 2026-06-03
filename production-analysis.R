# start by loading libraries needed for the project. this creates transparency to
# refer back to.

library(readr) # built in reading packages aren't perfect.
library(dplyr)
library(lubridate)
library(stringr)
library(ggplot2)
library(latex2exp)
#install.packages("latex2exp")

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
  summarize(sum(GAS_QUANTITY, na.rm = TRUE)) %>%
  mutate(dt = ym(paste0("20", str_remove(PERIOD_ID, "P$")))) %>%
  rename(gas = `sum(GAS_QUANTITY, na.rm = TRUE)`) %>%
  select(dt, gas)

write_csv(greene, "green_ogp_alt.csv") # export as csv file, making usable for people not using R

gg <- ggplot(greene) + 
  geom_line(aes(dt, gas/1e6)) +
  xlab("Date") + 
  ylab(TeX('Gas Quantity ($\\times 10^6 Mcf$)')) + 
  theme(panel.background = element_rect(fill = "white", color = "black"),
    axis.text = element_text(face = "plain", size = 14),
    axis.title = element_text(face = "plain", size = 14)) 

ggsave("GreenCnt_gas_prod.eps", gg, device = "eps")




  
  
#variable types:
  # double -> number
  # integer -> int
  # strings -> chr
  # logic -> true/false
  # factors = for categorization, property added to an existing variable
# array = an n-dimensional table
  #scalar - 0, vector - 1, matrices - 2
# data frame = an array with additional functionality based on its columns
# here, greene is a data frame. the command greene$PERIOD_ID[1] opens the data frame,
# accesses column PERIOD_ID, and returns the value at location 1.
# [] = defines position, () defines function



