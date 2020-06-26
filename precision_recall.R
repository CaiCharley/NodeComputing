# create precision recall curve from Prince Data
setwd("D:/Downloads/tmpcc/")

# load libraries
library(tidyverse)

ppis <- list.files(getwd(), pattern = "*.csv")

for (file in ppis) {
  assign(gsub(".csv$", "", file), read_csv(file))
}