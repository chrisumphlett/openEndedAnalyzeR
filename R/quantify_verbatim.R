#' Regress phrases on a quantitative variable from survey and produce plot
#' 
#' Regresses phrases on a quantitative variable from survey and produces a plot 
#' showing the associated change in quantitative if a particular phrase is used 
#' in the verbatim.
#'
#' @param surveydata Data frame with the survey data
#' @param tidydata Data frame with the unnested phrases (usually from tidy_verbatim())
#' @param id Unique survey id. By default, can be omitted if column is named 'id'
#' @param quant_var Numerical column in survey data frame on which phrases will be regressed
#' @param min_occur Number of times a phrase must occur to be included
#' @param min_phrases Force the plot to include at least this many phrases
#' @param max_phrases Maximum number of phrases to show in plot
#' @param pval_limit Threshold from 0 - 1. Phrases below the threshold will be highlighted
#' on the plot. Default is 0.1
#' @param xreg Optional list of columns from survey data frame to be used as additional
#' control variables in regression
#' @param remove_stop Do you want to remove common "stop-words"? Applies only to
#' single-word phrases ("unigrams"). If so, use TRUE
#' @import dplyr
#' @importFrom magrittr "%>%"
#' @import ggplot2
#' @import textdata
#' @importFrom broom "tidy"
#' @importFrom glue "glue"
#' @importFrom fastDummies "dummy_cols"
#' @importFrom stringr "str_replace_all"
#' @import tidytext
#' @export

quantify_verbatim <- function(surveydata,
                                tidydata,
                                id = id,
                                quant_var,
                                min_occur = 3,
                                min_phrases = 25,
                                max_phrases = 80,
                                pval_limit = 0.1,
                                xreg = c(),
                                remove_stop = TRUE) {
  
  phrase_freq <- create_phrasefreq({{tidydata}})
  
  # if unigrams, will remove stop phrases
  if (remove_stop == TRUE){
    phrase_freq <- phrase_freq %>%
      dplyr::filter(!phrase %in% tidytext::stop_words$word, phrase_freq >= min_occur)
  }
  
  phrase_wide <- {{tidydata}} %>%
    dplyr::inner_join(phrase_freq) %>%
    fastDummies::dummy_cols(select_columns = c("phrase"))

  quant_cols <- {{surveydata}} %>%
    rename(id = {{id}}) %>%
    dplyr::mutate(id = as.character(id)) %>%
    dplyr::select({{quant_var}}, id, {{xreg}})

  # now replace non-zeroes with ones to make dummies
  phrase_dummies <- phrase_wide %>%
    # dplyr::mutate(id = as.character(id)) %>%
    dplyr::select(-phrase) %>%
    dplyr::group_by(id, column_nm) %>%
    dplyr::summarise_all(sum) %>%
    dplyr::ungroup() %>%
    dplyr::mutate_if(is.numeric, ~ if_else(. > 0, 1, 0)) %>%
    dplyr::left_join(quant_cols) %>%
    dplyr::select(-c(id, phrase_freq))
 
  for (col in selected_cols) {
    col_lm <- phrase_dummies %>%
      dplyr::filter(column_nm == {{col}}) %>%
      dplyr::select(-column_nm)
    
    lm_formula <- glue::glue({{quant_var}}, " ~ .")
    cxi_lm_f <- lm(lm_formula, data = col_lm)
    
    cxi_lm_f2 <- broom::tidy(cxi_lm_f) %>%
      dplyr::mutate(term = stringr::str_replace_all(term, "`", ""))

    # analyze and plot significant phrases

    # select all phrases under pval of user-provided threshold
    # if that's less than min phrases, choose top n phrases; don't show more than max phrases

    ## count phrases under pval

    pval01 <- cxi_lm_f2 %>%
      dplyr::filter(substr(term, 1, 7) == "phrase_" & p.value <= {{pval_limit}}) %>%
      dplyr::pull()
    pval01_count <- length(pval01)
    # return(pval01)
    ## sort by pval and dplyr::select row_number = max of pval01_count or 25
    cxi_reg <- cxi_lm_f2 %>%
      dplyr::filter(substr(term, 1, 7) == "phrase_") %>%
      dplyr::arrange(p.value) %>%
      dplyr::filter(dplyr::row_number() <= max(min({{max_phrases}}, pval01_count), {{min_phrases}})) %>%
      dplyr::mutate(low_est = estimate - std.error,
             high_est = estimate + std.error,
             term2 = stringr::str_replace_all(term, "phrase_", ""),
             `Meets pval threshold` = if_else(p.value <= {{pval_limit}}, "Yes", "No")) %>%
      dplyr::select(estimate, low_est, high_est, term2, p.value)

    p <- ggplot2::ggplot(cxi_reg, aes(x= reorder(term2, -estimate))) +
      ggplot2::geom_pointrange(aes(y = estimate, ymin = low_est, ymax = high_est,
                                   color = p.value)) +
      ggplot2::coord_flip() +
      ggplot2::labs(title = "Associated change in quantitative response with presence of phrase in verbatim",
           subtitle = paste0("Quant column: ", glue::glue({{quant_var}}), "\nVerbatim column: ", {{col}}),
           y = "Change in quantitative resp.",
           x = "",
           caption = paste0("Regression model also includes: ", {{xreg}})) +
      ggplot2::theme_bw() +
      ggplot2::geom_hline(aes(yintercept = 0)) +
      ggplot2::scale_color_gradientn(colors = c("red", "grey25", "grey75"),
                                     # guide = "legend",
                                     values=c(0, {{pval_limit}}, 1))

    print(p)

  }

}
