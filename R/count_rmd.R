count_rmds <- function() {
  rmd_files <- list.files()
  rmd_files <- rmd_files[grepl(".[Rr]md$", rmd_files)]

  rmds_in_yml <- readLines("_bookdown.yml")
  rmds_in_yml <- rmds_in_yml  %>% stringr::str_remove_all("[',\\]]") %>%
    stringr::str_remove_all("#.*$") %>%
    stringr::str_trim()
  rmds_in_yml <- rmds_in_yml[grepl(".[Rr]md", rmds_in_yml)]

  cat("dupes in _bookdown.yml:\n")
  rmds_in_yml[duplicated(rmds_in_yml)]

  cat("Rmd files not in _bookdown.yml\n:")
  rmd_files[!(rmd_files %in% rmds_in_yml)]
}
