#' Easily edit bookdown chapter tiles
#'
#' In a large project project particularly with multiple authors,
#' the simple task of editing a chapter title can be difficult since
#' a few steps are required: find the right Rmd, open the file,
#' change the title, save it, close it. The \code{fixtitle()} function
#' simplifies the process by doing all this in one line of code. It can
#' be used for searching only, or searching and replacing.
#'
#' @param find full or partial title string to search for
#'
#' @param replace replacement string (optional)
#'
#' @param write logical indicating whether to change the title,
#' defaults to FALSE
#'
#' @export
#'
#' @examples
#' \dontrun{
#' fixtitle(find = "Instructions")    # search only
#'
#' fixtitle(find = "Instructions",    # search and replace test
#'          replace = "Submission instructions")
#'
#' fixtitle(find = "Instructions",    # search and replace actual
#'          replace = "Submission instructions",
#'          write = TRUE)
#' }
#'
#'
fixtitle <- function(find, replace = NULL, write = FALSE) {
  df <- findfile(find)
  if (nrow(df) == 0) stop ("No files found.")
  if (nrow(df) > 1) stop("Multiple files found.")
  if (!is.null(replace)) {
    file <- readLines(df$files[1], warn=FALSE)
    newfile <- sub(find, replace, file)
    print(newfile[1])
    if (write) {
      writeLines(newfile, df$files[1])
      message("File changed")
    } else {
      message("File not changed")
    }
  }
}

# internal function: findfile()

findfile <- function(find) {
  files <- list.files(pattern = ".Rmd")
  firstlines <- purrr::map_chr(files, readLines, n = 1)
  df <- tibble::tibble(files, firstlines) %>% dplyr::filter(grepl(find, firstlines, ignore.case = TRUE))
  print(df$files)
  print(df$firstlines)
  invisible(df)
}


