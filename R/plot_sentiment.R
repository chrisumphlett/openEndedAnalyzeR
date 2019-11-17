plot_response_sentiment <- function(data) {
  
}
b <- a %>%
  gather(sentiment, value, -id, -column_nm) %>%
  filter(!is.na(value)) %>%
  mutate(sentiment = str_replace(sentiment, "nrc_", ""))

ggplot(subset(b, 
              !sentiment %in% c("afinn_sentiment_index", "bing_sentiment_score") &
              value != 0)) +
  geom_histogram(aes(x = value)) +
  facet_grid(column_nm ~ sentiment, space = "free_y", scales = "free_y") +
  scale_x_continuous(breaks = scales::pretty_breaks()) +
  theme_bw() +
  labs(x = "Number of times sentiment appears in a survey response",
       y = "",
       title = "TITLE") +
  theme(panel.grid.minor.y = element_blank()) +
  scale_y_continuous(expand = c(0, 0))
