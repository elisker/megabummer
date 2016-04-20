#Documentation for twitteR: https://cran.r-project.org/web/packages/twitteR/twitteR.pdf


#SETTING AN .RPROFILE
#1. Figure out what your home directory is
path.expand("~")
#2. File > New File > R Script
#3. Save as .Rprofile in directory indicated in step 1.
#4. Declare variables for your keys, etc.


#library(ROAuth)
#library(streamR)
#library(devtools)
#library(rjson)
#library(bit64)
#library(httr)
library(twitteR)


setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)
#for leo:
setup_twitter_oauth(Sys.getenv("api_key"),
                    Sys.getenv("api_secret"),
                    Sys.getenv("access_token"),
                    Sys.getenv("access_token_secret"))


df <- searchTwitter("megabus,late",n=50)
View(twListToDF(df))

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
#download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile="cacert_emily.pem")
#credential$handshake(cainfo="cacert_emily.pem")
#setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)

