# compare time and RAM usage of MatLab and R PrInCE

setwd("D://Downloads/foster/mlbench/")
library(tidyverse)
library(magrittr)

# import ML benchmarks
files <- list.files(getwd(), ".benchmark$")

readbench <- function(file) {
  read_lines(file, skip = 1, n_max = 2) %>%
    parse_number() %>%
    t() %>%
    as_tibble()
}

mlbenchmarks <- map(files, ~ readbench(.)) %>%
  bind_rows() %>%
  bind_cols(str_remove(files, "-frac.*")) %>%
  setNames(c("time(s)", "RAM(kb)", "dataset")) %>%
  mutate(
    `RAM(MB)` = `RAM(kb)` / 1000,
    version = "ML",
    .keep = "unused"
  ) %>%
  relocate(dataset, .before = 1)

# import R benchmarks
rbenchmarks <- readRDS("rbenchmarks.rds") %>%
  filter(str_detect(expr, "NB-nmodels=1")) %>%
  transmute(
    dataset = str_remove(expr, "-classifier.*"),
    `time(s)` = median,
    `RAM(MB)` = `Peak_RAM_Used_MiB`,
    version = "R"
  )

# join benchmarks
benchmarks <- bind_rows(rbenchmarks, mlbenchmarks) %>%
  filter(!is.na(`time(s)`) || !is.na(`RAM(MB)`))

# calculate significance
timesig <- t.test(
  benchmarks %>%
    filter(version == "ML") %>%
    select(`time(s)`),
  benchmarks %>%
    filter(version == "R") %>%
    select(`time(s)`),
)

ramsig <- t.test(
  benchmarks %>%
    filter(version == "ML") %>%
    select(`RAM(MB)`),
  benchmarks %>%
    filter(version == "R") %>%
    select(`RAM(MB)`),
)

# compare time
ggplot(benchmarks, aes(version, `time(s)`, fill = version)) +
  geom_boxplot() +
  geom_point(alpha = 0.5, size = 1) +
  geom_line(alpha = 0.5, aes(group = dataset)) +
  ggtitle("Compare Elasped Runtime") +
  labs(x = "PrInCE Version", y = "Time Elapsed (s)")

ggsave("compare_time.png", width = 5, height = 5)

# compare RAM
ggplot(benchmarks, aes(version, `RAM(MB)`, fill = version)) +
  geom_boxplot() +
  geom_point(alpha = 0.5, size = 1) +
  geom_line(alpha = 0.5, aes(group = dataset)) +
  ylim(c(0, 6000)) + # !!!
  ggtitle("Compare Peak RAM Usage") +
  labs(x = "PrInCE Version", y = "Peak RAM Usage (MB)")

ggsave("compare_RAM.png", width = 5, height = 5)

# compare both
benchmarks_pivot <- bind_rows(rbenchmarks, mlbenchmarks) %>%
  pivot_longer(
    c(`time(s)`, `RAM(MB)`),
    names_to = "type",
    values_to = "value"
  ) %>%
  filter(!is.na(value))

ggplot(benchmarks_pivot, aes(type, value)) +
  geom_bar(stat = "identity", aes(fill = version)) +
  facet_wrap(~dataset)