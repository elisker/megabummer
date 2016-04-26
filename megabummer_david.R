#Documentation for twitteR: https://cran.r-project.org/web/packages/twitteR/twitteR.pdf

#library(ROAuth)
#library(streamR)
#library(devtools)
#library(rjson)
#library(bit64)
#library(httr)
library(twitteR)
library(dplyr)

# in .Rprofile
# options(api_key = "BLABLABLA",
#         api_secret = "BLABLABLA",
#         access_token = "BLABLABLA",
#         access_token_secret = "BLABLABLA")

# editing .Rprofile
# file.edit(".RProfile")

# restart R within Rstudio 
# .rs.restartR()

#The following (commented out) code is what Leo used to set up OAuth credentials. I don't think it's necessary to ever run these again.
#install.packages("ROauth")
#install.packages("streamR")
#install.packages("devtools")
#install.packages("rjson")
#install.packages("bit64")
#install.packages("httr")
#install.packages("twitteR")
#credential <- OAuthFactory$new(consumerKey=api_key,
#                               consumerSecret=api_secret,
#                               requestURL='https://api.twitter.com/oauth/request_token',
#                               accessURL='https://api.twitter.com/oauth/access_token',
#                               authURL='https://api.twitter.com/oauth/authorize')
#options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))
#download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile="cacert_e.pem")
#credential$handshake(cainfo="cacert_e.pem")
#setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)

# setup for Emily
setwd(dir = "/Users/eblisker/Documents/HSPH/Courses/2016 Spring/BIO 260/Final/megabummer")
setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)

# searching Twitter, English language tweets only
tweets <- searchTwitter("megabus", n = 3000, lang="en")

tweets_df <- bind_rows(lapply(tweets, as.data.frame))

library(devtools)
# install_github("juliasilge/tidytext")
library(tidytext)
# other text mining: tm, quanteda

#' Tidytext contains three lexicons for sentiment analysis are combined here in a tidy data frame.
#' The lexicons are the NRC Emotion Lexicon from Saif Mohammad and Peter Turney,
#' the sentiment lexicon from Bing Liu and collaborators, and the lexicon of
#' Finn Arup Nielsen. Words with non-ASCII characters were removed from the
#' lexicons.

#' @format A data frame with 23,165 rows and 4 variables:
#' \describe{
#'  \item{word}{An English word}
#'  \item{sentiment}{One of either positive, negative, anger, anticipation,
#'  disgust, fear, joy, sadness, surprise, trust, or \code{NA}. The Bing lexicon
#'  has positive/negative, the NRC lexicon has all options except \code{NA}, and
#'  the AFINN lexicon has only \code{NA}.}
#'  \item{lexicon}{The source of the sentiment for the word. One of either
#'  "nrc", "bing", or "AFINN".}
#'  \item{score}{A numerical score for the sentiment. This value is \code{NA}
#'  for the Bing and NRC lexicons, and runs between -5 and 5 for the AFINN
#'  lexicon.}
#' }
#'
#' @source \url{http://saifmohammad.com/WebPages/lexicons.html}
#' \url{https://www.cs.uic.edu/~liub/FBS/sentiment-analysis.html}
#' \url{http://www2.imm.dtu.dk/pubdb/views/publication_details.php?id=6010}

by_word <- tweets_df %>%
  filter(!isRetweet) %>%
  select(text, id, created) %>%
  unnest_tokens(word, text) 

by_word %>%
  count(word, sort = TRUE)

# original code from David

#bing <- sentiments %>%
#  filter(lexicon == "bing") %>%
#  select(-score)

# Supplemented bing sentiment lexicon with megabus specific sentiment. These additional negative and positive words
# were identified  by manually reviewing 2900 tweets on megabus queried on 4/20/2016. Negative and positive words not already
# included in the bing lexicon, such as those related to megabus and or transportation experience specifically, were added to the 
# lexicon.

# Notes on sentiment changes from bing: 
# uneventful: changed from negative to positive

library(tidyr)
library(readr)
megabus_lexicon <- read_csv("megabus_lexicon.csv")

# create new dataframe of bing and megabummer sentiments
bing_megabus <- megabus_lexicon %>%
  filter(lexicon %in% c("bing","megabummer")) %>%
  select(-score)

# create new dataframe calculating megabus sentiment
megabussentiment <- by_word %>%
  inner_join(bing_megabus) %>% 
  count(word, sentiment) %>% 
  spread(sentiment, n, fill = 0) %>% 
  mutate(sentiment = positive - negative)

# Export file as csv
write.csv(tweets_df, file = "megabus_tweets_df_4-26.csv")
write.csv(by_word, file = "megabus_by_word_4-26.csv")
