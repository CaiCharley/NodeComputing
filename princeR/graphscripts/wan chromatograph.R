setwd("C:/Users/Charley/OneDrive/Academic/Foster Lab/PrInCER/CC/data/wan")

library(tidyverse)
library(magrittr)

files <- list.files(getwd()) %>% map(~ readRDS(.) %>% rownames_to_column(var = "Protein"))
proteosome <- c("P60900", "F4JC97", "P25786", "P25789")

for (prot in proteosome) {
  for (i in seq_along(files)) {
    tmp <- files[[i]] %>% filter(str_detect(Protein, prot))
    if (!plyr::empty(tmp)) {
      assign(paste0(i, prot), tmp)
    }
  }
}

lst <- as.list(`3P25789`[-1])

ggplot() +
  geom_path(aes(seq_along(lst), as.numeric(lst)))