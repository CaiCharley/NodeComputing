setwd("C:/Users/Charley/OneDrive/Academic/Foster Lab/PrInCER/CC/data/wan")

library(tidyverse)
library(magrittr)

files <- list.files(getwd()) %>% map(~ readRDS(.))
proteosome <- c("P60900", "F4JC97")

for (prot in proteosome) {
  for (i in seq_along(files)) {
    tmp <- files[[i]] %>% filter(str_detect(Protein, prot))
    if (!plyr::empty(tmp)) {
      assign(paste0(i, prot), tmp)
    }
  }
}

tbl <- as_tibble(`5P60900`[-1]) %>% transpose() %>% mutate()
tbl1 <- tbl[[1]] %>% as_tibble()

# tbl <- tibble(count = `5P60900`[-1], fraction = seq_along(`5P60900`[-1]))

ggplot(tbl) +
  geom_path(aes(fraction, as.numeric(count)))