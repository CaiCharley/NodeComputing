# helper file for outer R script for bench

# lists input files
# void -> listOfString
get_inputs <- function() {
  file.path(base_dir, "dataML") %>%
    list.files("[^coreComplexes.txt]", full.names = T)
}

# lists other arguments passed to inner R script
# void -> listOfString
get_options <- function() {
  list(
    input_file = input_files # make sure input_file is first
  )
}

# make job specific modifications to default grid
# tibble -> tibble
modify_grid <- function(tbl) {
  tbl %>%
    mutate(
      basename = basename(input_file),
      output_file = file.path(output_dir, basename),
      fractions = str_extract(basename, "(?<=-frac=).*(?=-)"),
      replicates = str_extract(basename, "(?<=-rep=).*(?=\\.)")
    )
}

# checks to see if job is done
# list -> boolean
job_done <- function(job) {
    file.exists(job[["output_file"]])
}