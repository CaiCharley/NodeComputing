# create precision recall curve from PrinceML Data
setwd("/mnt/c/Users/User/Downloads/mlppis")
# setwd("/mnt/d/Downloads/tmpcc/")

# load libraries
library(tidyverse)
library(magrittr)
library(PrInCE)

# load data
files <- list.files(getwd(), pattern = "*.csv$") # !

read_with_pb <- function(file) {
  pb$tick()$print()
  read_csv(file, progress = F) %>%
    select(1, 5:8) %>%
    arrange(desc(`Interaction score (avg.)`)) %>%
    mutate(n = row_number())
}

pb <- progress_estimated(length(files))
ppi_list <- map(files, ~ read_with_pb(.)) %>%
  setNames(files)

# determine FP/TP/NA and calculate precision
ppi_list %<>% map(~ mutate(.,
  label =
    ifelse(!`Both proteins in Corum`, NA,
      ifelse(`Interaction in Corum`, 1, 0)
    )
))

with_precision <- map(ppi_list, ~ cbind(.,
  precision = calculate_precision(as_vector(select(., label)))
))

# bind rows and plot
ppis <- bind_rows(with_precision, .id = "filepath")

plot <- ggplot(ppis) +
  geom_path(aes(n, precision)) +
  facet_wrap(~filepath, nrow = 3) +
  ggtitle("Matlab Precision Recall") +
  ylab("Precision") +
  xlab("Predicted Interaction #") +
  scale_color_discrete(name = "Classifier")
ggsave(paste0(
  "/mnt/c/Users/User/Downloads/MLPrincePrecision", ".png"
), plot, device = "png", width = 14, height = 7)