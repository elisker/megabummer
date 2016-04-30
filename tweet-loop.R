#from http://www.gettinggeneticsdone.com/2012/07/plotting-frequency-of-twitter-hashtag.html?m=1
library(twitteR)
library(ggplot2)

tweets <- list()
dates <- paste("2016-04-",17:18,sep="") # need to go to 18th to catch tweets from 17th
for (i in 2:length(dates)) {
  print(paste(dates[i-1], dates[i]))
  tweets <- c(tweets, searchTwitter("megabus", since=dates[i-1], until=dates[i], n=1500))
}

# Convert the list to a data frame
tweets <- twListToDF(tweets)
tweets <- unique(tweets)
View(tweets)

# To ensure accuracy, make sure that there were no more than 1500 tweets in a single day.
# If there are 1500 on any single day, then you're truncating that day's tweets, and you'll
# need to try to get ROAuth (below) working.
tweets$date <- format(tweets$created, format="%Y-%m-%d")
table(tweets$date)

# @sciencestream is a spambot that's RT'ing everything on the #ISMB tag. Get rid of those.
tweets <- tweets[which(tweets$screenName!="sciencestream"), ]

# Make a table of the number of tweets per user
d <- as.data.frame(table(tweets$screenName))
d <- d[order(d$Freq, decreasing=T), ]
names(d) <- c("User","Tweets")
head(d)

# Plot the table above for the top 40
png("ismb-users.png", w=700, h=1000)
par(mar=c(5,10,2,2))
with(d[rev(1:40), ], barplot(Tweets, names=User, horiz=T, las=1, main="Top 40: Tweets per User", col=1))
dev.off()

# Plot the frequency of tweets over time in two hour windows
# Modified from http://michaelbommarito.com/2011/03/12/a-quick-look-at-march11-saudi-tweets/
minutes <- 120
ggplot(data=tweets, aes(x=created)) + 
  geom_histogram(aes(fill=..count..), binwidth=60*minutes) + 
  scale_x_datetime("Date") + 
  scale_y_continuous("Frequency")
ggsave(file='ismb-frequency.png', width=7, height=7, dpi=100)

# Use imagemagick to stitch together. Imagemagick must be installed in your path.
# montage ismb-frequency.png ismb-users.png -tile 1x -geometry -0-0 montage.png
# system("montage ismb-frequency.png ismb-users.png -tile 1x -geometry -0-0 montage.png")

#------------------------------------ Using the ROAuth package ---------------------------------------#
# ## If you can get this to work it's a bit more flexible and doesn't have the 1500/day limit as above.
# ## Using ROAuth will theoretically allow you to retrieve more than 1500 tweets with a single query.
# ## The current version on cran, 0.9.1, has known problems. Supposedly rolling back to 0.9.0 would work,
# ## and it did return TRUE after registerTwitterOAuth(cred) after the handshake, but I kept getting
# ## forbidden errors when trying to retrieve more than 1500. Also happened with version 0.9.2.
# 
# ## Current ROAuth from CRAN is 0.9.1. Has problems with handshake.
# install.packages("ROAuth")
# remove.packages("ROAuth")
# 
# ## Install 0.9.1 from CRAN (doesn't work)
# install.packages("ROAuth")
# 
# ## Using ROAuth 0.9.0 from source
# download.file("http://cran.r-project.org/src/contrib/Archive/ROAuth/ROAuth_0.9.0.tar.gz", destfile="ROAuth_0.9.0.tar.gz")
# install.packages("ROAuth_0.9.0.tar.gz", repos = NULL, type="source")
# library(ROAuth)
# 
# ## Using ROAuth 0.9.2 from source
# download.file("http://geoffjentry.hexdump.org/ROAuth_0.9.2.tar.gz", destfile="ROAuth_0.9.2.tar.gz")
# install.packages("ROAuth_0.9.2.tar.gz", repos = NULL, type="source")
# library(ROAuth)
# 
# ## The twitteR vignette has decent instructions
# vignette("twitteR")
# 
# cred <- OAuthFactory$new(consumerKey="your_consumer_key_here",
#                          consumerSecret="your_secret_key_here",
#                          requestURL="https://api.twitter.com/oauth/request_token",
#                          accessURL="http://api.twitter.com/oauth/access_token",
#                          authURL="http://api.twitter.com/oauth/authorize")
# cred$handshake()
# 
# ## You can save and load your credential object once you complete the handshake
# save(cred, file="~/Dropbox/code/misc/cred.RData")
# load("~/Dropbox/code/misc/cred.RData")
# 
# registerTwitterOAuth(cred)
# 
# tweets <- searchTwitter("#ISMB", since="2012-07-11", until="2012-07-18", n=9999)
# tweets <- twListToDF(tweets)
# ## Continue as above