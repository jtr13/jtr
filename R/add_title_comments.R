#' Add part and chapter title information as comments to _bookdown.yml or master documents
#'
#' It can be frustrating trying to remember which chapter is which in
#' a bookdown project with a lot of chapters. This function helps identify
#' the content of the chapter by adding part and title information in
#' the form of comments to the \code{_bookdown.yml} or master file.
#'
#' \code{add_title_comments()} searches through the specified
#' file line by line looking for lines with Rmd files. When it finds
#' one, it reads the first line of the actual Rmd file and then
#' adds the following to a copy of the file in memory:
#'
#' If the first line starts with "#" and contains "(PART)", the line is added
#' as a comment in its own line \emph{before} the Rmd line.
#'
#' If the first line starts with "#" but doesn't contain "(PART)", the line
#' is added as a comment after the Rmd file name.
#'
#' If the first line does not start with "#" but the Rmd filename is Index (or
#' index), it reads the full file and looks for the first line beginning with "#"
#' to add as above. (Note that if comments appear in the `Index.Rmd`` before
#' the title line, they will be misinterpreted as titles.)
#'
#'
#' For master files, the child chapter title is added as a comment inside the code chunk, and \code{echo=FALSE} is added to the chunk options.
#'
#' WARNING: all existing comments are erased so proceed with caution.
#' As a safety measure, the function does not overwrite the specified file, but rather returns an object which can be
#' written to file with \code{writeLines()}. See examples.
#'
#' Real example: \url{https://github.com/jtr13/1201/blob/master/_bookdown.yml}
#'
#' @export
#'
#' @param file defaults to \code{_bookdown.yml}
#'
#' @param master indicates that the file is a master document with children, defaults to \code{FALSE}
#'
#' @examples
#' \dontrun{
#' writeLines(add_title_comments(), "test.yml")
#' writeLines(add_title_comments(), "_bookdown.yml") # be careful!
#' writeLines(add_title_comments("master_workshops.Rmd", master = TRUE), "test.yml")
#' }
#'



add_title_comments <- function(file="_bookdown.yml", master=FALSE) {
  # https://stackoverflow.com/questions/24812271/r-functions-print-warning-only-on-first-call-of-function
    if(is.null(getOption("add_title_comments.message"))) {
      message("Be aware that this function erases all existing comments first.")
    options(add_title_comments.message = TRUE)
    }
  file <- strip_comments(file, master) %>%
    purrr::map_chr(process_line, master = master)
  if (nchar(file[length(file)]) > 0) file <- c(file, "\n")
  if (master) {
    pre <- "<!--"
    post <- "-->"
  } else {
    pre <- "#"
    post <- ""
  }
  # need to fix this so HTML version gets erased
  c(file, paste(pre, "Comments added by jtr::add_title_comments()", post))
}

# internal functions: process_line(), strip(), strip_comments()

process_line <- function(line, master) {
  if (stringr::str_detect(line, ".[Rr]md")) {
    filename <- stringr::str_match(line, "[A-z0-9_-]+.[Rr]md") %>%
      as.character()
    # to do: meaningful error message if filename isn't found
    # (not: Error in file(con, "r") : cannot open the connection)
    firstline <- readLines(filename, n = 1)
    if (stringr::str_sub(firstline, 1, 1) == "#") {
      if (stringr::str_detect(firstline, "PART")) {
        titleline <- readLines(filename, n = 3)
        line <- paste0("\n", firstline, "\n", line, " ", titleline[3])
      } else {
        if (master) {
          if (!grepl("echo=FALSE", line)) line <- stringr::str_replace(line, "\\}", ", echo=FALSE\\}")
          separator <- "\n"
        } else {
          separator <- " "
        }
        line <- paste0(line, separator, firstline)
      }
    } else if (grepl("^[Ii]ndex.[Rr]md$", filename)) {
      filecontents <- readLines(filename)
      title <- filecontents[grep("#", filecontents)[1]]
      line <- paste(line, title)
    }
  }
  invisible(line)
}

strip <- function(line) {
  stringr::str_remove(line, paste0("\\s*#.*"))
}

strip_comments <- function(file, master) {
  # first completely remove all lines that start with # except for first line
  newfile <- readLines(file)
  newfile <- c(newfile[1], newfile[-1][!grepl("^#", newfile[-1])])
  # remove middle of line comments
  c(newfile[1], purrr::map_chr(newfile[-1], strip))
  }

