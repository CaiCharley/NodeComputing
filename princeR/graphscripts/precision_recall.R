# create precision recall curve from Prince Data
setwd("/home/caic/projects/rrg-ljfoster-ab/caic/princeR/ppis/")
# setwd("/mnt/d/Downloads/tmpcc/")

# load libraries
library(tidyverse)
library(magrittr)

# load data
files <- list.files(getwd(), pattern = "*.csv.gz") # !

ppi_list <- map(files, ~ read_csv(.,
  n_max = 100000,
  col_types = cols(
    X1 = col_character(),
    protein_A = col_character(),
    score = col_double(),
    label = col_logical(),
    precision = col_double()
  )
) %>%
  mutate(n = row_number())) %>%
  setNames(files)
ppis <- bind_rows(ppi_list, .id = "filepath")

ppis %<>% mutate(
  nmodels = as.numeric(str_extract(filepath, "(?<=nmodels=).*(?=.csv)")),
  classifier = str_extract(filepath, "(?<=classifier=).*(?=-)"),
  dataset = (sub("-.*", "", filepath))
) %>%
  select(-filepath, -protein_A, -protein_B) %>%
  filter(n %% 10 == 0)

# plot

# Graph coloured by classifier, facet by dataset
ppis %<>% filter(nmodels == 1)
plot <- ggplot(ppis) +
  geom_path(aes(n, precision, color = as.factor(classifier))) +
  facet_wrap(~dataset, nrow = 2) +
  ggtitle("Nmodels = 1") +
  ylab("Precision") +
  xlab("Predicted Interaction #") +
  scale_color_discrete(name = "Classifier")
ggsave(paste0(
  "~/projects/rrg-ljfoster-ab/caic/princeR/ppis/graphs/",
  "colbyclassifierfacbydata", ".png"
), plot, device = "png", width = 7, height = 14)


# Graph for each dataset, coloured nmodel, facet by classifier
# by_dataset <- ppis %>% group_split(dataset)
# for (data in by_dataset) {
#   dataname <- data$dataset[1]
#   plot <- ggplot(data) +
#     geom_path(aes(n, precision, color = as.factor(nmodels)), se = F) +
#     facet_wrap(~classifier, nrow = 2) +
#     ggtitle(dataname) +
#     ylab("Precision") +
#     xlab("Predicted Interaction #") +
#     scale_color_discrete(name = "n Models")
#   ggsave(paste0(
#     "~/projects/rrg-ljfoster-ab/caic/princeR/ppis/graphs/",
#     dataname, ".png"
#   ), plot, device = "png")
# }