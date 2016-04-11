#https://cran.r-project.org/web/packages/twitteR/twitteR.pdf

library(ROAuth)
library(streamR)
library(devtools)
library(rjson)
library(bit64)
library(httr)
library(twitteR)


api_key <- 'PJPULmfGZhIdxIkUo9U0uGyc1'
api_secret <- 'iJBUs2X40D3KKnM1pveTRvpDz5WAYFyrTJ2LP9Qbj0vzcGLhP8'
access_token <- '219841618-9HSysAb8iZR0mw9G76bCjMuL70hTCqnw90m9LkxS'
access_token_secret <- 'T3OSXcuzozq4Jd3Y6ISEJZtYJBIU8FY5PgzbUver5GotT'

df <- searchTwitter("megabus,late",n=50)
View(twListToDF(df))



#The following (commented out) code is what Leo used to set up OAuth credentials. I don't think it's necessary to ever run this again.
#credential <- OAuthFactory$new(consumerKey=api_key,
#                               consumerSecret=api_secret,
#                               requestURL='https://api.twitter.com/oauth/request_token',
#                               accessURL='https://api.twitter.com/oauth/access_token',
#                               authURL='https://api.twitter.com/oauth/authorize')
#options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))
#download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile="cacert.pem")
#credential$handshake(cainfo="cacert.pem")

