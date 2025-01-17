# openEndedAnalyzeR
Tidy analysis of open-ended survey question responses

The package is being built to make it easy to do generalized forms of analysis that I have found useful for open ended survey responses. The workflow one should be able to utilize with the help of the package is to call one function to prepare the data, and then a second function to do an analysis and get an output (visualization or data frame).

## Installation
You can install the current version by using `devtools::install_github("chrisumphlett/openEndedAnalyzeR")`. Currently it does not work for R 3.5 and greater. I am working on troubleshooting this. I developed the package with R 3.5! 

## What is available now
The basic building block is the `tidy_verbatim` function: This will take the survey data and transform it from one row per survey, to one row per survey response/question/phrase. This must be done first before utilizing any of the analysis functions.

The first version of the package had one analysis function: `create_phraseclouds()`. This allows the user to make wordclouds easily not only for single words, but for phrases of length 'n' words ("n-grams").

I have now added additional functions: 
* `response_sentiment`: Assign sentiments to single words
* `plot_response_sentiment`: Creates plots to do explatory analysis on the sentiment of single words
* `quantify_verbatim`: Regress phrases on a quantitative variable from survey and produce a plot showing the associated change in quantitative if a particular phrase is used in the verbatim

## What I am working on
I already have several features I plan on adding where I have performed the analysis outside the package and just need to convert it a form suitable for the package. These include:
* Allowing the user to specify if they want stop words excluded from phrases. This works by first breaking the text down into single words, removing stop words, concatenating back together and then breaking into phrases.
* Combining qualitative and quantitative responses. For businesses there are a variety of trademarked indices to measure how the business relates to its customers, like Net Promoter Score (NPS) and Customer Experience Index (CXi). The idea here is to assess how the voice of the customer in the open ended responses is associated with changes in NPS and CXi (or any other outcome variable).
    * For single words, can look at the relationship between sentiment and an outcome variable.
    * For n-grams of any length, can use as variables in a linear regression to estimate associated change in outcome variable when customer uses a particular phrase.

## How you can help
* This is my first package. I know that I am not doing some things according to best practices!
* Test and utilize and help refine the functions above as they are added.
* Either create new functions for other types of analyses or provide ideas and use cases.
* Help create vignette(s) and documentation.