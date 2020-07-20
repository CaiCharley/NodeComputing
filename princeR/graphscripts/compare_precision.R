# copmpare precision of NB classifier between ML and R
setwd("/home/caic/projects/rrg-ljfoster-ab/caic/princeR/")
# setwd("/mnt/d/Downloads")

library(tidyverse)
library(magrittr)

# load ML ppis
mlfiles <- list.files(file.path(getwd(), "mlppis"),
  full.names = T,
  pattern = "*.csv$"
) # !

readml_with_pb <- function(file) {
  pb$tick()$print()
  read_csv(file, progress = F) %>%
    select(1, 5:8) %>%
    arrange(desc(`Interaction score (avg.)`)) %>%
    mutate(n = row_number(), .before = 1)
}

pb <- progress_estimated(length(mlfiles))
mlppi_list <- map(mlfiles, ~ readml_with_pb(.)) %>%
  setNames(basename(mlfiles))

# determine FP/TP/NA and calculate precision
mlppi_list %<>% map(~ mutate(.,
  label =
    ifelse(!`Both proteins in Corum`, NA,
      ifelse(`Interaction in Corum`, 1, 0)
    )
))

mlppis <- map(mlppi_list, ~ cbind(.,
  precision = calculate_precision(as_vector(select(., label)))
))
mlppis %<>%
  bind_rows(.id = "filepath") %>%
  mutate(
    version = "ML",
    dataset = str_extract(filepath, "^.*(?=\\-frac.*)")
  )

# load R ppis
rfiles <- list.files(file.path(getwd(), "ppis"),
  full.names = T,
  pattern = "classifier=NB-nmodels=1.csv.gz$"
) # !

readr_with_pb <- function(file) {
  pb$tick()$print()
  df <- read_csv(file,
    n_max = 100000,
    col_types = cols(
      X1 = col_character(),
      protein_A = col_character(),
      score = col_double(),
      label = col_logical(),
      precision = col_double()
    ),
    progress = F
  ) %>%
    mutate(n = row_number(), .before = 1) %>%
    select(-protein_A, -protein_B)
  names(df)[names(df) == "X1"] <- "Unique interactions"
  return(df)
}

pb <- progress_estimated(length(rfiles))
rppi_list <- map(rfiles, ~ readr_with_pb(.)) %>%
  setNames(basename(rfiles))
rppis <- bind_rows(rppi_list, .id = "filepath")

rppis %<>%
  mutate(
    nmodels = as.numeric(str_extract(filepath, "(?<=nmodels=).*(?=.csv)")),
    classifier = str_extract(filepath, "(?<=classifier=).*(?=-)"),
    dataset = (sub("-.*", "", filepath)),
    version = "R"
  ) %>%
  select(-filepath) %>%
  filter(n %% 10 == 0)

# combine ppis and plot
ppis <- bind_rows(mlppis, rppis)
ggplot(ppis) +
  geom_path(aes(n, precision, colour = version)) +
  facet_wrap(~dataset, nrow = 3) +
  ggtitle("Matlab Precision Recall") +
  ylab("Precision") +
  xlab("Predicted Interaction #") +
  scale_color_discrete(name = "Classifier")
ggsave(paste0(
  "~/OneDrive/compare_precision", ".png"
), last_plot(), device = "png", width = 14, height = 7)