#' Create phrase frequency
#'
#' Creates data frame with count of occurences of each phrase in each column.
#' This function will be called by analysis functions, not intended to be
#' used directly.
#'
#' @param data Data frame with the unnested phrases
#' @import dplyr
#' @import magrittr

create_phrasefreq <- function(data) {

  phrase_freq_all <- {{data}} %>%
    dplyr::group_by(column_nm, phrase) %>%
    dplyr::summarise(phrase_freq = n()) %>%
    dplyr::ungroup()

  return(phrase_freq_all)
}