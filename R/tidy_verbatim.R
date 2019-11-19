#' Create a tidy data frame with phrases from each column
#'
#' This function creates a long table with one row per phrase and column.
#' It allows the user to set the n-gram parameter (how many words per phrase)
#' and then utilizes the tidytext library to unnest each verbatim response.
#' Any NA responses are dropped.
#'
#' It's recommended that you select your columns into a vector outside
#' of the function call, because you will probably want to re-use that
#' list with other functions in the package.
#'
#' @param data Data frame with the survey data
#' @param id Unique survey id. By default, can be omitted if column is named 'id'
#' @param cols List of column(s) to be unnested. By default, can be omitted if 
#' using vector named 'selected_cols'
#' @param n Length of phrases, in words. By default is 1 (unigrams)
#' @return A data frame with one row per phrase and column, with survey id.
#' @import dplyr
#' @importFrom magrittr "%>%"
#' @import tidytext
#' @import tibble
#' @export
#' @examples
#' data(bloom_survey)
#' selected_cols <- c("q19verb", "q20verb")
#' surveys <- tidy_verbatim(bloom_survey, n = 3)

tidy_verbatim <- function(data, id = id, cols = selected_cols, n = 1) {
  x <- {{data}} %>%
    dplyr::select(id, cols) %>%
    dplyr::mutate(id = as.character(id))

  basedf <- tibble::tibble(id = character(),
               column_nm = character(),
               phrase = character())

  for (column_i in cols) {
    x2 <- x %>%
      dplyr::select(id, {{column_i}}) %>%
      tidytext::unnest_tokens(output = "phrase", input = {{column_i}}, token = "ngrams", n = n) %>%
      dplyr::mutate(column_nm = {{column_i}}) %>%
      dplyr::filter(!is.na(phrase))

    basedf <- dplyr::bind_rows(basedf, x2)
  }
  #
  return(basedf)
}



