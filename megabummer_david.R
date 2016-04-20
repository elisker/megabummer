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
setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)

# searching Twitter
tweets <- searchTwitter("megabus", n = 3000)

tweets_df <- bind_rows(lapply(tweets, as.data.frame))

library(devtools)
# install_github("juliasilge/tidytext")
library(tidytext)
# other text mining: tm, quanteda

by_word <- tweets_df %>%
  filter(!isRetweet) %>%
  select(text, id, created) %>%
  unnest_tokens(word, text) 

by_word %>%
  count(word, sort = TRUE)

library(tidyr)
bing <- sentiments %>%
  filter(lexicon == "bing") %>%
  select(-score)

megabussentiment <- by_word %>%
  inner_join(bing) %>% 
  count(word, sentiment) %>% 
  spread(sentiment, n, fill = 0) %>% 
  mutate(sentiment = positive - negative)

# Export file as csv
write.csv(by_word, file = "megabus_4-20.csv")