# analyze PrInCER's benchmark results

library(tidyverse)

benchmarks <- readRDS("/mnt/d/Downloads/benchmarks.rds")
names(benchmarks)[1] <- "filepath"

benchmarks %<>% mutate(
  nmodels = as.numeric(sub(".*nmodels=", "", filepath)),
  classifier = str_extract(filepath, "(?<=classifier=).*(?=-)"),
  dataset = (sub("-.*", "", filepath))
)
col_order <- c(
  "filepath", "dataset", "classifier", "nmodels",
  "Peak_RAM_Used_MiB", "min", "lq", "mean", "median", "uq", "max", "neval"
)
benchmarks <- benchmarks[, col_order]

group_by(benchmarks, dataset, nmodels, classifier)

time <- ggplot(benchmarks) +
  geom_boxplot(aes(mean, reorder(filepath, mean)))

ram <- ggplot(benchmarks) +
  geom_boxplot(aes(Peak_RAM_Used_MiB, reorder(filepath, Peak_RAM_Used_MiB)))

ram_facet <- ggplot(benchmarks) +
  geom_boxplot(aes(Peak_RAM_Used_MiB, reorder(classifier, Peak_RAM_Used_MiB))) +
  facet_grid(nmodels ~ dataset)

time_facet <- ggplot(benchmarks) +
  geom_boxplot(aes(mean, reorder(classifier, median))) +
  coord_cartesian(xlim = c(0, 5000)) +
  facet_grid(nmodels ~ dataset)

ggsave("/mnt/d/Downloads/bench_ram.png", ram, device = "png")
ggsave("/mnt/d/Downloads/bench_timefacet.png", time_facet, device = "png")