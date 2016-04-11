#https://cran.r-project.org/web/packages/twitteR/twitteR.pdf

install.packages("ROAuth")
install.packages("streamR")
install.packages("devtools")
install.packages("rjson")
install.packages("bit64")
install.packages("httr")
install.packages("twitteR")


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
credential <- OAuthFactory$new(consumerKey=api_key,
                               consumerSecret=api_secret,
                               requestURL='https://api.twitter.com/oauth/request_token',
                               accessURL='https://api.twitter.com/oauth/access_token',
                               authURL='https://api.twitter.com/oauth/authorize')
options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))
download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile="cacert_ali.pem")
credential$handshake(cainfo="cacert_ali.pem")

api_key <- 'PJPULmfGZhIdxIkUo9U0uGyc1'
api_secret <- 'iJBUs2X40D3KKnM1pveTRvpDz5WAYFyrTJ2LP9Qbj0vzcGLhP8'
access_token <- '219841618-9HSysAb8iZR0mw9G76bCjMuL70hTCqnw90m9LkxS'
access_token_secret <- 'T3OSXcuzozq4Jd3Y6ISEJZtYJBIU8FY5PgzbUver5GotT'

#Ali's token and verifier
#oauth_token=EZCwRAAAAAAAuhfhAAABVAZMGAQ&
#oauth_verifier=5sMkZoSZji28tvl6yD3hJ2Ka32I703RF

credential$handshake(cainfo="cacert.pem")

