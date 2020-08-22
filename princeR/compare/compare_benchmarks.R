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

# join and plot benchmarks
benchmarks <- bind_rows(rbenchmarks, mlbenchmarks) %>%
  pivot_longer(c(`time(s)`, `RAM(MB)`), names_to = "type", values_to = "value")

ggplot(benchmarks, aes(type, value)) +
  geom_bar(stat = "identity", aes(fill = version)) +
  facet_wrap(~dataset)