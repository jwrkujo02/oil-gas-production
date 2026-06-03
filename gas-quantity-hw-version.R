# load libraries
library(readr) 
library(dplyr)
library(lubridate)
library(stringr)
library(ggplot2)
library(latex2exp)

# load data from directory
prod21 <- read_csv("OilGasProduction2021.csv")
prod22 <- read_csv("OilGasProduction2022.csv")
prod23 <- read_csv("OilGasProduction2023.csv")

# combine all data sets into one variable and remove individuals
prod <- rbind(prod21, prod22, prod23)
rm(prod21, prod22, prod23)

# create new variable for Butler County, get summary of gas quantity and fix dt labels
butler <- prod %>% 
     filter(COUNTY == "Butler") %>%
     group_by(PERIOD_ID) %>% 
     summarize(sum(GAS_QUANTITY, na.rm = TRUE)) %>%
     mutate(dt = ym(paste0("20", str_remove(PERIOD_ID, "P$")))) %>%
     rename(gas = `sum(GAS_QUANTITY, na.rm = TRUE)`) %>%
     select(dt, gas)

# specify variable for ggplot, add all desired features
gg <- ggplot(butler) + 
     geom_line(aes(dt, gas/1e6)) +
     xlab("Date") + 
     ylab(TeX('Gas Quantity ($\\times 10^6 Mcf$)')) + 
     theme(panel.background = element_rect(fill = "white", color = "black"),
           axis.text = element_text(face = "plain", size = 14),
           axis.title = element_text(face = "plain", size = 14)) 

# save to working directory
ggsave("ButlerCnt_gas_prod.pdf", gg, device = "pdf")


