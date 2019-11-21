#' Assign sentiments to unigrams using nrc, afinn and bing dictionaries
#'
#' Creates data frame with one row per survey, selected question, and summarized 
#' sentiment. Three sentiment dictionaries are used; the first time this function runs
#' you will need to download the dictionary (there will be a prompt in the console).
#' 
#' Afinn and Bing dictionaries will each produce a single score which represents the 
#' overall polarity of sentiment. These dictionaries score each applicable word for 
#' its negativity or positivity; Afinn scores from -5 to 5, while Bing just labels as 
#' negative/positive. The Afinn index is the sum of all individual word scores and the 
#' Bing index represents the count of positive words minus count of negative words.
#' 
#' The NRC dictionary assigns words to one of several sentiments, such as anger or joy.
#' The summarized values will represent the count of words assigned to each sentiment
#' on the response and question level.
#'
#' @param tidy_data Data frame with the unnested phrases (usually from tidy_verbatim())
#' @import dplyr
#' @importFrom magrittr "%>%"
#' @import tidytext
#' @import textdata
#' @import tidyr
#' @export

response_sentiment <- function(tidy_data) {
  
  afinn_by_survey <- {{tidy_data}} %>%
    dplyr::left_join(tidytext::get_sentiments("afinn"), by = c("phrase" = "word")) %>%
    dplyr::filter(!is.na(value)) %>%
    dplyr::rename(sentiment = value) %>%
    dplyr::group_by(id, column_nm) %>%
    dplyr::summarise(afinn_sentiment_index = sum(sentiment)) %>%
    dplyr::ungroup()
  
  # bing_by_survey <- {{tidy_data}} %>%
  #   dplyr::left_join(tidytext::get_sentiments("bing"), by = c("phrase" = "word")) %>%
  #   dplyr::filter(!is.na(sentiment)) %>%
  #   dplyr::group_by(id, column_nm, sentiment) %>%
  #   dplyr::summarise(sentiment_count = n()) %>%
  #   dplyr::ungroup() %>%
  #   tidyr::spread(sentiment, sentiment_count, fill = 0) %>%
  #   dplyr::mutate(bing_sentiment_score = positive - negative) %>%
  #   dplyr::select(-c(negative, positive))
  
  nrc_by_survey <- {{tidy_data}} %>%
    dplyr::left_join(tidytext::get_sentiments("nrc"), by = c("phrase" = "word")) %>%
    dplyr::filter(!is.na(sentiment)) %>%
    dplyr::rename(nrc = sentiment) %>%
    dplyr::group_by(id, column_nm, nrc) %>%
    dplyr::summarise(sentiment_count = n()) %>%
    dplyr::ungroup() %>%
    tidyr::spread(nrc, sentiment_count, fill = 0, sep = "_")
  
  sentiment_by_survey <- afinn_by_survey %>%
    # dplyr::left_join(bing_by_survey) %>%
    dplyr::left_join(nrc_by_survey)
  
  return(sentiment_by_survey)
}
