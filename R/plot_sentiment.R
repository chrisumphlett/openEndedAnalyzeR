#' Creates exploratory plots of the assignment of sentiments to words
#'
#' Creates and prints ggplots showing how the sentiment assignment works.
#' One or more plots can be displayed depending on the user preference.
#' 
#' The first plot, "sentiment_histogram", shows a histogram for each sentiment
#' and question. It shows the distribution of the sentiment by the frequency
#' of occurences
#'
#' @param data Data frame assigned sentiments (typically from response_sentiment())
#' @import dplyr
#' @importFrom magrittr "%>%"
#' @import tidyr
#' @import ggplot2
#' @import scales
#' @export

plot_response_sentiment <- function(data) {
  sentiment_long <- {{data}} %>%
    tidyr::gather(sentiment, value, -id, -column_nm) %>%
    dplyr::filter(!is.na(value)) %>%
    dplyr::mutate(sentiment = str_replace(sentiment, "nrc_", ""))
  
  ggplot(subset(sentiment_long, !sentiment %in% c("afinn_sentiment_index",
                                     "bing_sentiment_score",
                                     "negative",
                                     "positive"))) +
    ggplot2::geom_histogram(aes(x = value)) +
    ggplot2::facet_grid(column_nm ~ sentiment, space = "free_y", scales = "free_y") +
    ggplot2::scale_x_continuous(breaks = scales::pretty_breaks()) +
    ggplot2::theme_classic() +
    ggplot2::labs(x = "Number of times NRC sentiment appears in a survey response",
         y = "",
         title = "Count of survey responses that include sentiment, by frequency") +
    ggplot2::theme(panel.grid.major.y = element_line(color = "grey", size = 0.5)) +
    ggplot2::scale_y_continuous(expand = c(0, 0), breaks = scales::pretty_breaks())
}





# ggplot(subset(b, 
#               !sentiment %in% c("afinn_sentiment_index", "bing_sentiment_score", "negative", "positive"))) +
#   # geom_histogram(aes(x = value)) +
#   stat_ecdf(aes(x = value), geom = "step") +
#   facet_grid(column_nm ~ sentiment, space = "free_y", scales = "free_y") +
#   scale_x_continuous(breaks = scales::pretty_breaks()) +
#   theme_classic() +
#   labs(x = "Number of times sentiment appears in a survey response",
#        y = "",
#        title = "TITLE") +
#   theme(panel.grid.major.x = element_line(color = "grey", size = 0.5)) +
  # scale_y_continuous(expand = c(0, 0), labels = scales::percent)
