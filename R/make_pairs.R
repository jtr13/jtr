#' Make problem set pairings based on section
#'
#' file must be a file downloaded from CourseWorks people section with `group_name` column
#'
#' @param file path to file
#'
#' @param prefix prefix for group names
#'
#' @return data frame with new pairs column
#'
#' @examples
#'
#' df <- make_pairs("~/Downloads/PSet\ 3.csv")
#'
#' @export
#'
make_pairs <- function(file = "~/Downloads/PSet\ 3.csv", prefix= "PSet\ 3") {
  df <- readr::read_csv(file)
  df <- df %>% mutate(preference = str_extract(sections, "[a-z]+_[a-z]+_*[a-z]*")) %>%
    mutate(preference = replace_na(preference, "no_preference"))
  set.seed(5702)

  # using base R since I don't care if number of items to replace is not a multiple of replacement length

  nremote <- nrow(df[df$preference == "meet_remotely",])
  df$group_name[df$preference == "meet_remotely"] <- paste(prefix, str_pad(rep(sample(nremote/2), 2), 2, pad = "0"))

  nperson <- nrow(df[df$preference == "meet_in_person",])
  df$group_name[df$preference == "meet_in_person"] <- paste(prefix, str_pad(rep(sample(nperson/2), 2) + round(nremote/2), 2, pad = "0"))

  nnopref <- nrow(df[df$preference == "no_preference",])
  df$group_name[df$preference == "no_preference"] <- paste(prefix, str_pad(rep(sample(nnopref/2), 2) + round(nremote/2) + round(nperson/2), 2, pad = "0"))

  write_csv(df, file)
}

