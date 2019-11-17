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
#' @import magrittr
#' @import tidytext
#' @import textdata
#' @import tidyr

response_sentiment <- function(tidy_data) {
  
  afinn_by_survey <- {{tidy_data}} %>%
    left_join(get_sentiments("afinn"), by = c("phrase" = "word")) %>%
    filter(!is.na(value)) %>%
    rename(sentiment = value) %>%
    group_by(id, column_nm) %>%
    summarise(afinn_sentiment_index = sum(sentiment)) %>%
    ungroup()
  
  bing_by_survey <- {{tidy_data}} %>%
    left_join(get_sentiments("bing"), by = c("phrase" = "word")) %>%
    filter(!is.na(sentiment)) %>%
    group_by(id, column_nm, sentiment) %>%
    summarise(sentiment_count = n()) %>%
    ungroup() %>%
    spread(sentiment, sentiment_count, fill = 0) %>%
    mutate(bing_sentiment_score = positive - negative) %>%
    select(-c(negative, positive))
  
  nrc_by_survey <- {{tidy_data}} %>%
    left_join(get_sentiments("nrc"), by = c("phrase" = "word")) %>%
    filter(!is.na(sentiment)) %>%
    rename(nrc = sentiment) %>%
    group_by(id, column_nm, nrc) %>%
    summarise(sentiment_count = n()) %>%
    ungroup() %>%
    spread(nrc, sentiment_count, fill = 0, sep = "_")
  
  sentiment_by_survey <- afinn_by_survey %>%
    left_join(bing_by_survey) %>%
    left_join(nrc_by_survey)
  
  return(sentiment_by_survey)
}
