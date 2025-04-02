#' Record what you're doing now to stay on track
#'
#' @returns
#' @export
#'
#' @examples
#'
#' \dontrun{
#' record()
#' }
#'
record <- function() {
  what <- "not done"
  sink("~/Downloads/stuff.txt")
  while(what != "done") {
    what <- readline(prompt = "What are you doing? ")
    cat(paste(format(Sys.time(), "%H:%M"), what, "\n"))
  }
  sink()
}
