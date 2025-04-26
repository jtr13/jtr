#' Generate a Random Month and Year
#'
#' Returns a randomly selected month and year between February 1967 and the current month.
#'
#' @return A character string in the format "Month YYYY", e.g., "October 1993".
#' @examples
#' random_month_year()
#'
#' @export
random_month_year <- function() {
  # Define the start and end dates
  start_date <- as.Date("1967-02-01")
  end_date <- Sys.Date()

  # Generate a sequence of months between the two dates
  all_months <- seq(from = start_date, to = end_date, by = "month")

  # Randomly pick one date
  random_date <- sample(all_months, 1)

  # Return formatted string
  format(random_date, "%B %Y")
}
