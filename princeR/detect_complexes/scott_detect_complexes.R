# generate heatmap with detect_complexes on scott dataset
setwd("/mnt/d/Downloads/scottdata")

# load data as individual list element
goldstd <- readRDS("goldstd.rds")
file_names <- list.files(pattern = "[^goldstd.rds]")
files <- map(file_names, ~ readRDS(.)) %>% 
  setNames(str_replace(file_names, ".rds", "")) %>% 
  unlist(recursive = F)

# detect complexes
goldcmplx <- goldstd[lengths(goldstd) > 2]

scott_complexes <- map(files, ~ detect_complexes(., goldcmplx) %>% 
    na.omit())
