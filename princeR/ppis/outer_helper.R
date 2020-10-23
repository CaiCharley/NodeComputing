# helper file for outer R script for ppis

# lists input files
# void -> listOfString
get_inputs <- function() {
  file.path(base_dir, "scottdata") %>%
    list.files("[^goldstd.rds]", full.names = T)
}

# lists other arguments passed to inner R script
# void -> listOfString
get_options <- function() {
  list(
    input_file = input_files, # make sure input_file is first
    classifier = c("NB", "SVM", "LR", "ensemble"),
    nmodels = c(1)
  )
}

# make job specific modifications to default grid
# tibble -> tibble
modify_grid <- function(tbl) {
  tbl
}

# checks to see if job is done
# list -> boolean
job_done <- function(job) {
  input_filename <- job[["input_file"]] %>%
    basename() %>%
    gsub("\\.rds$", "", .) # ! input file type
  expected_output <-
    paste0(
      input_filename,
      paste0(optionprefix, job[-1], collapse = ""), # job[1] is input file
      ".csv.gz" # ! output file extension
    )
  file.path(output_dir, expected_output) %>%
    file.exists()
}