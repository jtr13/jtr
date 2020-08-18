#' Move bookdown (PART) to another chapter
#'
#' @param oldfile file that currently contains (PART) info
#'
#' @param newfile file to get (PART) info
#'
#' @export
#'
#' @examples
#' \dontrun{
#' movepart(oldfile = "Chapter3.Rmd", newfile = "Chapter4.Rmd")
#' }
#'

movepart <- function(oldfile, newfile) {
  old <- readLines(oldfile)
  firstline <- readLines(oldfile, n = 1)
  if (!grepl("\\(PART\\)", firstline)) stop (paste("No part information found in ", firstline))
  partinfo <- old[1:2]
  writeLines(old[3:length(old)], oldfile)
  new <- readLines(newfile)
  writeLines(c(partinfo, new), newfile)
}
