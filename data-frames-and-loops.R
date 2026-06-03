# previously, we went through prod data set to select only Greene county, sort by month,year
# and then generated sum of gas production to examine that data.

# today, we will learn how to use a for loop to increase functionality

library(readr)
library(ggplot2)
library(lubridate)
library(latex2exp)

# read in the data
prod21 <- read_csv("OilGasProduction2021.csv")
prod22 <- read_csv("OilGasProduction2022.csv")
prod23 <- read_csv("OilGasProduction2023.csv")

# function useful for combining data into one data frame (if using multiple sets)
prod <- rbind(prod21, prod22, prod23)
# rm() function removes the variables specified.
rm(prod21, prod22, prod23)

# a bit about loops:
     # for - preferred, hard to make infinite. step counter is integrated
     # do - easier to make infinite. do something a bunch of times
     # while - easiest to make infinite. do when something is a certain value


PERIOD_ID <- array(NA, dim = nrow(prod))
lat <- array(NA, dim = nrow(prod))
lon <- array(NA, dim = nrow(prod))
spud <- array(NA, dim = nrow(prod))
GAS_QUANTITY <- array(NA, dim = nrow(prod))
greene <- data.frame(PERIOD_ID, lat, lon, spud, GAS_QUANTITY)
rm(PERIOD_ID, lat, lon, spud, GAS_QUANTITY)

j <- 1
for (i in 1:nrow(prod)) {
     if (prod$COUNTY[i] == "Greene") {
          greene$PERIOD_ID[j] <- prod$PERIOD_ID[i]
          greene$lat[j] <- prod$LATITUDE_DECIMAL[i]
          greene$lon[j] <- prod$LONGITUDE_DECIMAL[i]
          greene$spud[j] <- prod$SPUD_DATE[i]
          greene$GAS_QUANTITY[j] <- prod$GAS_QUANTITY[i] # copying everything that is greene
          j <- j + 1
     }
}

greene <- greene[1:(j-1),]
ggas <- array(0, dim = c(3, 12))


for (i in 1:nrow(greene)) {
     yr <- substr(greene$PERIOD_ID[i], 1, 2) # first row is 2021, position will be yr - 20
     yr <- as.numeric(yr)
     mo <- substr(greene$PERIOD_ID[i], 3, 5) # tricky, need to assign all the months to #s
     if (mo == "JAN") {mn <- 1
     } else if (mo == "FEB") {mn <- 2
     } else if (mo == "MAR") {mn <- 3
     } else if (mo == "APR") {mn <- 4
     } else if (mo == "MAY") {mn <- 5
     } else if (mo == "JUN") {mn <- 6
     } else if (mo == "JUL") {mn <- 7
     } else if (mo == "AUG") {mn <- 8
     } else if (mo == "SEP") {mn <- 9
     } else if (mo == "OCT") {mn <- 10
     } else if (mo == "NOV") {mn <- 11
     } else if (mo == "DEC") {mn <- 12} 
     if (is.na(greene$GAS_QUANTITY[i])) {
          gas <- 0
     } else {
          gas <- greene$GAS_QUANTITY[i]
     }
     ggas[(yr-20), mn] <- ggas[(yr-20), mn] + gas
}

# convert ggas from wide data format to long data format
dt <- array(NA, dim = (nrow(ggas) * ncol(ggas)))
gas <- array(NA, dim = (nrow(ggas) * ncol(ggas)))
greene2 <- data.frame(dt, gas)
rm(dt, gas)

for (i in 1:nrow(ggas)) {
     for (j in 1:ncol(ggas)) {
          greene2$dt[12*(i-1) + j] <- ymd(paste0(2020+i, "/", j, "/01"))
          greene2$gas[12*(i-1) + j] <- ggas[i, j]
          
     }
}



ggplot(greene2) + 
     geom_line(aes(x = as_date(dt), y = gas/1e6)) +
     xlab("Date") + 
     ylab(TeX('Gas Production ($\\times 10^6 Mcf$)')) + 
     theme(panel.background = element_rect(fill = "white", color = "black"),
           axis.text = element_text(face = "plain", size = 14),
           axis.title = element_text(face = "plain", size = 14)) 


