#Documentation for twitteR: https://cran.r-project.org/web/packages/twitteR/twitteR.pdf

#library(ROAuth)
#library(streamR)
#library(devtools)
#library(rjson)
#library(bit64)
#library(httr)
library(twitteR)
library(dplyr)
library(ggplot2)

#in .Rprofile
# options(api_key = "BLABLABLA",
#         api_secret = "BLABLABLA",
#         access_token = "BLABLABLA",
#         access_token_secret = "BLABLABLA")

# editing .Rprofile
# file.edit(".Rprofile")

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

#### SEARCH TWITTER ####

# searching Twitter, English language tweets only
tweets <- searchTwitter("megabus", n = 3500, lang="en")

tweets_df <- bind_rows(lapply(tweets, as.data.frame))

#### DATA CLEANING ANDEXPLORATORY ANALYSIS ####

# explore favorited, retweet, and retweeted counts
table(tweets_df$favorited)
table(tweets_df$retweeted)
table(tweets_df$isRetweet)

# filter out retweets
tweets_df <- tweets_df %>%
  filter(!isRetweet) %>%filter(!isRetweet)

# add date and time
tweets_df$date <- format(tweets_df$created, format="%Y-%m-%d")
tweets_df$time <- format(tweets_df$created, format="%H:%M:%S") # NEED TO look into time zone issues if any

# make table of number of tweets per day
table(tweets_df$date)

# explore number of tweets per user
prolific.tweeters <- tweets_df %>% 
  group_by(screenName) %>%
  summarise(tweets = n()) %>%
  arrange(desc(tweets)) 

# Histogram of number of tweets
ggplot(filter(prolific.tweeters, tweets>0), aes(tweets)) + 
  geom_histogram(binwidth = 1) + ylab("Number of tweets per user")

# Plot the frequency of tweets over time in two hour windows
# Modified from http://michaelbommarito.com/2011/03/12/a-quick-look-at-march11-saudi-tweets/
minutes <- 120
ggplot(data=tweets_df, aes(x=created)) + 
  geom_histogram(aes(fill=..count..), binwidth=60*minutes) + 
  scale_x_datetime("Date") + 
  scale_y_continuous("Frequency")

# really clunky look at tweets over 24 hour period
tweets_df$time <- as.POSIXct(tweets_df$time, format="%H:%M:%S")
brks <- trunc(range(tweets_df$time), "hours")
hist(tweets_df$time, breaks=seq(brks[1], brks[2]+3600, by="30 min") )

###### SENTIMENT ANALYSIS ######

library(devtools)
#install_github("juliasilge/tidytext")
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

# original code from David below

#bing <- sentiments %>%
#  filter(lexicon == "bing") %>%
#  select(-score)

# Supplemented bing sentiment lexicon with 57 megabus or transportation specific sentiments. These additional negative and positive words
# were identified  by manually reviewing a random sample from 2900 tweets on megabus queried on 4/20/2016, following similar methods described 
# here: http://www.wired.com/2015/02/best-worst-public-transit-systems-according-twitter/. Negative and positive words not already
# included in the bing lexicon, such as those related to megabus and or transportation experience specifically, were added to the 
# lexicon.

# Notes on sentiment changes from bing: 
# uneventful: changed from negative to positive
# cheap: changed from negative to positive

library(tidyr)
library(readr)

by_word <- tweets_df %>%
  select(text, id, created) %>%
  unnest_tokens(word, text) 

# look at most commonly tweeted words
by_word_count <- by_word %>%
  count(word, sort = TRUE)

#Ali's Directory
#setwd(dir = "/Users/ablajda/desktop/DataScience/megabummer")

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
  mutate(score = positive - negative)

# trying out a new type of sentiment analysis based on this http://www.r-bloggers.com/sentiment-analysis-on-donald-trump-using-r-and-tableau/
# NEED TO FINISH: http://www.r-bloggers.com/sentiment-analysis-on-donald-trump-using-r-and-tableau/
positives = bing_megabus %>%
  filter(sentiment == "positive") %>%
  select(word)

negatives = bing_megabus %>%
  filter(sentiment == "negative") %>%
  select(word)

# try using this resource to display avg sentiment score of tweets over time: 
# https://www.credera.com/blog/technology-insights/open-source-technology-insights/twitter-analytics-using-r-part-3-compare-sentiments/

# create wordcloud, resource: http://www.rdatamining.com/docs/twitter-analysis-with-r
install.packages("wordcloud")
install.packages("tm")
install.packages("SnowballC")
install.packages("RColorBrewer") # color palettes

library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)

View(by_word)
word_list <- by_word %>% select(word)

word_list_negatives <- subset(word_list, word %in% negatives$word)
View(word_list_negatives)

word_list_positives <- subset(word_list, word %in% positives$word)
View(word_list_positives)

# 5 Easy Steps to a Word Cloud: http://www.sthda.com/english/wiki/text-mining-and-word-cloud-fundamentals-in-r-5-simple-steps-you-should-know#the-five-main-steps-for-creating-a-word-cloud-using-r-software

# Negative Word Cloud
word_list_negatives <- Corpus(VectorSource(word_list_negatives))
inspect(word_list_negatives)

toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
word_list_negatives <- tm_map(word_list_negatives, toSpace, "/")
word_list_negatives <- tm_map(word_list_negatives, toSpace, "@")
word_list_negatives <- tm_map(word_list_negatives, toSpace, "\\|")

#Build a term-document matrix
dtm <- TermDocumentMatrix(word_list_negatives)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

# Word Cloud (Negative)
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

##### Positive Word Cloud #####
word_list_positives <- Corpus(VectorSource(word_list_positives))
inspect(word_list_positives)

toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
word_list_positives <- tm_map(word_list_positives, toSpace, "/")
word_list_positives <- tm_map(word_list_positives, toSpace, "@")
word_list_positives <- tm_map(word_list_positives, toSpace, "\\|")
word_list_positives <- tm_map(word_list_positives, removeWords, c("megabus", "the", "and", "https", "you", "t.co", "for", "this", "bus", "that")) 

#Build a term-document matrix
dtm <- TermDocumentMatrix(word_list_positives)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

# Word Cloud (Positive)
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

####### END WORD CLOUD #######

# Word Clouds by Date
View(by_word)

# gganimate setup
.rs.restartR()

# Load gganimate
devtools::install_github("dgrtwo/gganimate")

library(gapminder)
library(ggplot2)
theme_set(theme_bw())

View(gapminder)

p <- ggplot(gapminder, aes(gdpPercap, lifeExp, size = pop, color = continent, frame = year)) +
  geom_point() +
  scale_x_log10()

library(gganimate)

gg_animate(p)



m <- as.matrix(word_list)
# calculate the frequency of words and sort it by frequency
word.freq <- sort(rowSums(m), decreasing = T)
# colors
pal <- brewer.pal(9, "BuGn")[-(1:4)]

m <- as.matrix(tdm)
# calculate the frequency of words and sort it by frequency
word.freq <- sort(rowSums(m), decreasing = T)
# colors
pal <- brewer.pal(9, "BuGn")[-(1:4)]
#plotting the wordcloud
wordcloud(words = names(word.freq), freq = word.freq, min.freq = 3,
          random.order = F, colors = pal)

# Export file as csv
write.csv(tweets_df, file = "megabus_tweets_df_4-26.csv")
write.csv(by_word, file = "megabus_by_word_4-26.csv")
