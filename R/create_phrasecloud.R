#' Create wordclouds with phrases from selected columns
#'
#' This function creates a phrasecloud for each selected text column. Phraseclouds
#' can be wordclouds, if the phrase length was set as 1, but can also be n-grams--
#' phrases with length of "n" words.
#' Several optional parameters give control over how many phrases are rendered in
#' the phrasecloud. Each has a reasonable default setting to start.
#'
#' It requires a data frame with specific columns. The easiest way to create this is
#' with tidy_verbatim().
#'
#' It's recommended that you select your columns into a vector outside
#' of the function call. This is then passed into both tidy_verbatim() and
#' create_phrasecloud().
#'
#' @param data Data frame with tidied, unnested phrases
#' @param cols List of column(s) to be unnested
#' @param min_occur Number of times a phrase must occur to be included
#' @param min_phrases Force the phrasecloud to include at least this many phrases
#' @param pct_phrases Only show top "n" percent of phrases in phrasecloud
#' @param max_phrases Maximum number of phrases to show in phrasecloud
#' @param remove_stop Do you want to remove common "stop-words"? Applies only to
#' single-word phrases ("unigrams"). If so, use TRUE
#' @import dplyr
#' @import tidytext
#' @importFrom magrittr "%>%"
#' @export

create_phraseclouds <- function(data,
                              cols = selected_cols,
                              min_occur = 3,
                              min_phrases = 25,
                              pct_phrases = 1.00,
                              max_phrases = 100,
                              remove_stop = TRUE) {

  phrase_freq <- create_phrasefreq({{data}})

  # if unigrams, will remove stop phrases
  if (remove_stop == TRUE){
    phrase_freq <- phrase_freq %>%
      dplyr::filter(!phrase %in% stop_words$word, phrase_freq >= min_occur)
  }

  for (column_i in cols) {
    phrase_freq2 <- dplyr::filter(phrase_freq, column_nm == {{column_i}})

    n_phrases <- max(min_phrases, min(max_phrases, pct_phrases * nrow(phrase_freq2)))
    pal2 <- RColorBrewer::brewer.pal(8,"Dark2") # color scheme

    freq_cloud <- phrase_freq2 %>%
      dplyr::arrange(-phrase_freq) %>%
      dplyr::filter(row_number() <= n_phrases)

    # https://stackoverflow.com/questions/15224913/r-add-title-to-phrasecloud-graphics-png
    layout(matrix(c(1, 2), nrow = 2), heights = c(1, 4))
    par(mar=rep(0, 4))
    plot.new()
    text(x=0.5, y=0.5, paste0("Wordcloud of ", {{column_i}}))
    wordcloud::wordcloud(freq_cloud$phrase, freq_cloud$phrase_freq, random.order = FALSE, colors = pal2)
  }

}
