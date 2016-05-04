#Documentation for twitteR: https://cran.r-project.org/web/packages/twitteR/twitteR.pdf

library(dplyr)
library(ggplot2)
library(ggthemes)
library(readr)

#### DATA CLEANING AND EXPLORATORY ANALYSIS ####

# load older tweets and merge datasets
options(digits = 22) # to prevent tweet id from truncating

#full data set
tweets_df_all <- read_csv("q12015-q22016 copy.csv")
#get a subset of random lines from this to work with

#subset of full data set
#tweets_df_all <- tweets_df_all[sample(1:nrow(tweets_df_all), 10000, replace=FALSE),]


names(tweets_df_all) <- c("id","username","text","date","geo","retweets","favorites","mentions","hashtags")
tweets_df_all$date2 <- format(tweets_df_all$date, format="%m-%d-%y")

tweets_df_all <- tweets_df_all %>% 
  filter(date2 != "12-31-14")

tweets_df_all$time <- format(tweets_df_all$date, format="%H:%M:%S") 
tweets_df_all <- tweets_df_all %>%
  mutate(month = months(as.Date(date2,'%m-%d-%y'))) %>%
  mutate(weekend = weekdays(as.Date(date2,'%m-%d-%y'))) %>%
  mutate(weekend_binary = ifelse(weekend == "Saturday"|weekend=="Sunday"|weekend=="Friday", 1, 0))

#ggplot(tweets_df_all, aes(unclass(date))[1]) + geom_density()

# filter out duplicates
tweets_df_all <- tweets_df_all %>%
  distinct(id)
#nrow(tweets_df_all)
tweets_df_all <- tweets_df_all %>%
  distinct(text)

# explore number of tweets per user including megabus handles
#prolific_tweeters_all <- tweets_df_all %>% 
#  group_by(username) %>%
#  summarise(tweets = n()) %>%
#  arrange(desc(tweets)) 

# filter out tweets from megabus operators
tweets_df_all = tweets_df_all[!grepl("megabus|megabusuk|MegabusHelp|megabusit|megabusde|megabusGold", tweets_df_all$username),]

# explore number of tweets per user excluding megabus handles
#prolific_tweeters <- tweets_df_all %>% 
#  group_by(username) %>%
#  summarise(tweets = n()) %>%
#  arrange(desc(tweets))

# Histogram of number of tweets
#ggplot(filter(prolific_tweeters, tweets>0), aes(tweets)) + 
#  geom_histogram(binwidth = 1) + xlab("Number of megabus tweets per user") + ylab("Frequency") + theme_hc()

# Plot the frequency of tweets over time in two hour windows
# Modified from http://michaelbommarito.com/2011/03/12/a-quick-look-at-march11-saudi-tweets/


ggplot(data=tweets_df_all, aes(x=as.Date(date2,'%m-%d-%y'))) + 
  geom_histogram(aes(fill=..count..), binwidth=1) + 
  scale_x_date("Date") + 
  scale_y_continuous("Frequency")

#The outlying days with high tweet volume are:
tweets_df_all %>% group_by(date2) %>% count(date2, sort = TRUE) %>% filter(n>500)
#The outlying days with low tweet volume are:
tweets_df_all %>% group_by(date2) %>% count(date2, sort = TRUE) %>% filter(n<100)

#TODO (Leo) get rid of dates april 23 2016 and after

#TODO (Emily) a version of the above chart that is smooth
#TODO (Ali) include the articles in the Rmd, do what Rafa does
#There were some outliers in tweet volume. We investigated Google to see if anything notable happened on days where tweet volume exceeded 500.
#April 13, 2015: ‘Hero’ passenger subdues gunman who may have tried to take over Megabus https://www.washingtonpost.com/news/morning-mix/wp/2015/05/13/hero-passenger-subdues-gunman-who-may-have-tried-to-take-over-megabus/
#May 13, 2015: 19 injured in Megabus crash on I-65 in Indiana http://www.usatoday.com/story/news/nation/2015/04/13/megabus-crash-indiana/25707085/
#February 21, 2016: The Day My Megabus Caught Fire https://www.google.com/url?sa=t&rct=j&q=&esrc=s&source=newssearch&cd=1&ved=0ahUKEwjw6rqk18DMAhWLbT4KHacICLYQqQIIHCgAMAA&url=http%3A%2F%2Fwww.nytimes.com%2F2016%2F02%2F22%2Ftravel%2Fthe-day-my-megabus-caught-fire.html&usg=AFQjCNGn4yumdmj3dXBvebbspQf6saQwBw&sig2=x1ans1gQL3jwUhE4p5FEEg


###### SENTIMENT ANALYSIS ######

library(devtools)
#install_github("juliasilge/tidytext")
library(tidytext)
# other text mining: tm, quanteda

library(tidyr)
library(readr)

by_word <- tweets_df_all %>%
  select(text, id, date, date2, time, weekend, weekend_binary) %>%
  unnest_tokens(word, text) 



# look at most commonly tweeted words
by_word_count <- by_word %>%
  count(word, sort = TRUE) 

megabus_lexicon <- read_csv("megabus_lexicon.csv")

# create new dataframe of bing and megabummer sentiments
bing_megabus <- megabus_lexicon %>%
  filter(lexicon %in% c("bing","megabummer")) %>%
  select(-score)

# join tweets with sentiment and add score column
mb_sentiment <- by_word %>%
  inner_join(bing_megabus) %>%
  mutate(score = ifelse(sentiment == "positive", 1, -1))

# calculate score for each tweet
options(digits = 22)
library(data.table)
dt <- data.table(mb_sentiment)


mb_sentiment_tweet <- unique(dt[,list(score_tweet = sum(score), freq = .N, date, weekend_binary, date2, weekend, time), by = c("id")] )

mb_sentiment_date <- unique(mb_sentiment_tweet[,list(score_date = round(mean(score_tweet),2), freq = .N, weekend_binary), by = c("date2")] )
mb_sentiment_date <- mb_sentiment_date %>% filter(freq<500)
mb_sentiment_holidays <- mb_sentiment_date %>% 
  mutate(holiday = ifelse(date2 == "01-01-15" |
                            date2 == "01-19-15" |
                            date2 == "02-14-15" |
                            date2 == "02-16-15" |
                            date2 == "05-25-15" |
                            date2 == "09-07-15" |
                            date2 == "10-12-15" |
                            date2 == "10-31-15" |
                            date2 == "11-11-15" |
                            date2 == "11-26-15" |
                            date2 == "12-25-15" |
                            date2 == "01-01-16" |
                            date2 == "01-18-16" |
                            date2 == "02-14-16" |
                            date2 == "02-15-16",1,
                          0
                            ))
#mb_sentiment_day <- unique(mb_sentiment_tweet[,list(score_date = round(mean(score_tweet),2), freq = .N), by = c("weekend")] )

# summary stats
library(Hmisc)
#mb_sentiment_tweet %>% filter(score_tweet == 4)
#tweets_df_all %>% filter(id==574126622481211392) %>% select(text)
describe(mb_sentiment_tweet)
describe(mb_sentiment_date)

# As you can see, the line graph of the sentiment score over time is not that useful. 
ggplot(data=mb_sentiment_tweet, aes(x=date, y=score_tweet)) + 
  geom_line()

# Instead of fitting a line, we can stratify by date and compute the mean sentiment score,referred to as _bin smoothing_. 
# Smoothing is useful for this analysis as it is designed to estimate $f(x)$ when the shape is unknown, but assumed to be _smooth_.  
# When we group the data points into strata, days in this case, that are expected to have similar expectations and calculate the average
# or fit a simple model in each strata. We assume the curve is approximately constant within the bin  and that all the sentiment scores 
# in that bin have the same expected value. 
ggplot(data=mb_sentiment_tweet, aes(x=date, y=score_tweet)) + 
  geom_smooth()

# histogram of all sentiment scores (tweet)
ggplot(data=mb_sentiment_tweet, aes(score_tweet)) + 
  geom_histogram(binwidth = 1)

# histogram of all sentiment scores (day)
ggplot(data=mb_sentiment_date, aes(score_date)) + 
  geom_histogram(binwidth = 0.1)

# Adding month in case we want to look at variations by month
mb_sentiment_date$month <- month(as.POSIXlt(mb_sentiment_date$date2, format="%m-%d-%y"))

#ggplot(data=mb_sentiment_date, aes(x=month, y=score_date)) + 
#  geom_boxplot()#does this work? I had to write geom_boxplot()

# boxplots and violin plots of sentiment by date
#ggplot(mb_sentiment_tweet, aes(x=date2, y=freq, group=date2)) +
#  geom_boxplot(aes(fill=date2)) +
#  geom_jitter(colour="gray40",
#              position=position_jitter(width=0.2), alpha=0.3) 

# bar chart of average score (NEED to fix y axis scale)
#meanscore <- tapply(mb_sentiment_tweet$score_tweet, mb_sentiment_tweet$date2, mean)
#df = data.frame(day=names(meanscore), meanscore=meanscore)
#df$day <- reorder(df$day, df$meanscore)
#ggplot(df, aes(x=day, y=meanscore)) +
#  geom_bar(stat = "identity") +
#  scale_y_continuous("Frequency")


#Hyp. #1: tweet sentiment on low volume days = on high volume days
h1.lm <- lm(score_date ~ freq, data = mb_sentiment_date)
summary(h1.lm)
#REJECT THE NULL (STRONGLY), conclude that tweet sentiment on low volume days > tweet sentiment on high volume days
# The volume of tweets on a given day is a statistically significant predictor of the average daily sentiment score and
# that for every additional tweet, we would expect a 0.001 decrease in average daily sentiment score.

# graph tweet sentiment as a function of tweet volume
ggplot(data=mb_sentiment_date, aes(x=freq, y=score_date)) + 
  geom_line()

score_date.res = resid(h1.lm)
plot(mb_sentiment_date$freq, score_date.res, 
ylab="Residuals", xlab="Number of Tweets", 
main="Tweets and Megabus Sentiment")
abline(0,0)
                

# Evaluate homoscedasticity
# non-constant error variance test
ncvTest(h1.lm)
# We fail to reject the null hypothesis of homoskedastic errors

# Diagnostic plots
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page 
plot(h1.lm)

#install.packages("car")
library(car)
# Normality of Residuals
# qq plot for studentized resid
qqPlot(h1.lm, main="QQ Plot")
# distribution of studentized residuals
#library(MASS)
sresid <- studres(h1.lm) 
hist(sresid, freq=FALSE, 
     main="Distribution of Studentized Residuals")
xfit<-seq(min(sresid),max(sresid),length=40) 
yfit<-dnorm(xfit) 
lines(xfit, yfit)

#Hyp. #2: sentiment weekend = sentiment weekday 
#(looking at weekend and nonweekend as two large groups of tweet scores)
#Just looking at average tweet score for all weekend days versus all weekdays
the_weekend = mb_sentiment_tweet %>% filter(weekend_binary == 1)
not_the_weekend = mb_sentiment_tweet %>% filter(weekend_binary == 0)
var.test(the_weekend$score_tweet,not_the_weekend$score_tweet)#variances are equal if p-value > 0.05
t.test(the_weekend$score_tweet,not_the_weekend$score_tweet)#,var.equal = TRUE)
#Conclusion: When looking at all weekend vs weekday tweet scores as one large group, 
#mean tweet score on weekend is significantly less than mean tweet score on non-weekend, 
#*without* stratifying on tweet volume.
ggplot(mb_sentiment_tweet, aes(x=weekend_binary, y=score_tweet, group=weekend_binary)) +
  geom_boxplot(aes(fill=weekend_binary)) +
  geom_jitter(colour="gray40",
              position=position_jitter(width=0.2), alpha=0.3) 

#Hyp. #3: sentiment weekend = sentiment weekday 
#(looking at weekend and nonweekend as two large groups of DAILY AVERAGES
#of gweet scores)
the_weekend_date = mb_sentiment_date %>% filter(weekend_binary == 1)
not_the_weekend_date = mb_sentiment_date %>% filter(weekend_binary == 0)
var.test(the_weekend_date$score_date,not_the_weekend_date$score_date)#variances are equal if p-value > 0.05
t.test(the_weekend_date$score_date,not_the_weekend_date$score_date)#,var.equal = TRUE)
#Conclusion: Mean of daily tweet scores on weekend is significantly less than 
#mean tweet score on non-weekend, *without* stratifying on tweet volume.
ggplot(mb_sentiment_date, aes(x=weekend_binary, y=score_date, group=weekend_binary)) +
  geom_boxplot(aes(fill=weekend_binary)) +
  geom_jitter(colour="gray40",
              position=position_jitter(width=0.2), alpha=0.3) 

#Hyp. #4: sentiment weekend = sentiment weekday when stratifying on freq.
#[multiple linear regression]
fit <- lm(score_date ~ weekend_binary + freq, data=mb_sentiment_date)
summary(fit) # show results
#DO NOT REJECT THE NULL, conclusion: that the association between weekend and tweet sentiment
#is *entirely* due to volume
#I'm not sure how to graphically represent muliple linear regression.

#Hyp. #5: tweet volume weekend = tweet volume weekday

var.test(the_weekend_date$freq,not_the_weekend_date$freq)
t.test(the_weekend_date$freq,not_the_weekend_date$freq)

#REJECT THE NULL (STRONGLY), conclude that tweet volume weekend != tweet volume weekday
ggplot(mb_sentiment_date, aes(x=weekend_binary, y=freq, group=weekend_binary)) +
  geom_boxplot(aes(fill=weekend_binary)) +
  geom_jitter(colour="gray40",
              position=position_jitter(width=0.2), alpha=0.3) 


#Hyp. #6: sentiment holiday = sentiment !holiday
#Without stratifying on volume
holiday = mb_sentiment_holidays %>% filter(holiday == 1)
not_holiday = mb_sentiment_holidays %>% filter(holiday == 0)
var.test(holiday$score_date,not_holiday$score_date)#variances are equal if p-value > 0.05
t.test(holiday$score_date,not_holiday$score_date,var.equal = TRUE)
#There is no association between holiday and tweet sentiment WITHOUT stratifying on volume.

#Stratifying on volume
#[multiple linear regression]
fit <- lm(score_date ~ holiday + freq, data=mb_sentiment_holidays)
summary(fit) # show results
#There is no association between holiday and tweet sentiment WITH stratifying on volume.
ggplot(mb_sentiment_holidays, aes(x=holiday, y=freq, group=holiday)) +
  geom_boxplot(aes(fill=holiday)) +
  geom_jitter(colour="gray40",
              position=position_jitter(width=0.2), alpha=0.3) 

#mb_sentiment_tweet_vol <- mb_sentiment_tweet %>% group_by(date2) %>% count(date2)
#tweet_quartiles <- mb_sentiment_tweet_vol%>%
#  summarise(`25%`=quantile(n, probs=0.25),
#            `50%`=quantile(n, probs=0.5),
#            `75%`=quantile(n, probs=0.75),
#            avg=mean(n),
#            n=n())
#first_quartile <- tweet_quartiles[1][[1]]
#second_quartile <- tweet_quartiles[1][[2]]
#third_quartile <- tweet_quartiles[1][[3]]
#mb_sentiment_tweet_vol$quartile <- 1
#for(i in 1:nrow(mb_sentiment_tweet_vol)) {
#  if(mb_sentiment_tweet_vol$n[i]>=third_quartile) {
#    mb_sentiment_tweet_vol$quartile[i] <- 4
#  } else if(mb_sentiment_tweet_vol$n[i]>second_quartile) {
#    mb_sentiment_tweet_vol$quartile[i] <- 3
#  } else if(mb_sentiment_tweet_vol$n[i]>first_quartile) {
#    mb_sentiment_tweet_vol$quartile[i] <- 2
#  } else {
#    mb_sentiment_tweet_vol$quartile[i] <- 1
#  }
#}
#mb_sentiment_tweet_quartiles <- left_join(mb_sentiment_tweet,mb_sentiment_tweet_vol,by="date2") %>% select(-n)
#mb_sentiment_tweet_quartiles$quartile = factor(mb_sentiment_tweet_quartiles$quartile)
#ggplot(mb_sentiment_tweet_quartiles, aes(x=quartile, y = score_tweet)) +
#  geom_boxplot(fill = "grey80", colour = "blue") +
#  scale_x_discrete() + xlab("Quartile") +
#  ylab("Tweet score")
#tweet_volume_model <- lm(score_tweet ~ quartile, data = mb_sentiment_tweet_quartiles)
#summary(tweet_volume_model)







#Vis 1: word network thing (Ali)
#Vis 2: word cloud positive
#Vis 3: word cloud negative
#Vis 4: word cloud of all XX words in positive tweets
#Vis 5: word cloud of all XX words in negative tweets

#wbar
#ybar

#xbar = mean(mb_sentiment_tweet$score_tweet)           # sample mean 
#> mu0 = 15.4             # hypothesized value 
#> s = 2.5                # sample standard deviation 
#> n = 35                 # sample size 
#> t = (xbar−mu0)/(s/sqrt(n)) 
#> t                      # test statistic 
#
#> alpha = .05 
#> t.half.alpha = qt(1−alpha/2, df=n−1) 
#> c(−t.half.alpha, t.half.alpha)
  
# create new dataframe calculating megabus sentiment Remove below?
#megabussentiment_overall <- by_word %>%
#  inner_join(bing_megabus) %>% 
#  count(word, sentiment) %>% 
#  spread(sentiment, n, fill = 0) %>% 
#  mutate(score = positive - negative)

# NEED TO FINISH: http://www.r-bloggers.com/sentiment-analysis-on-donald-trump-using-r-and-tableau/
positives = bing_megabus %>%
  filter(sentiment == "positive") %>%
  select(word)

negatives = bing_megabus %>%
  filter(sentiment == "negative") %>%
  select(word)

## gganimate: by day (e.g., mondays)

# try using this resource to display avg sentiment score of tweets over time: 
# https://www.credera.com/blog/technology-insights/open-source-technology-insights/twitter-analytics-using-r-part-3-compare-sentiments/

# create wordcloud, resource: http://www.rdatamining.com/docs/twitter-analysis-with-r
#install.packages("wordcloud")
#install.packages("tm")
#install.packages("SnowballC")

#install.packages("RColorBrewer") # color palettes

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

# Word Clouds by Weekend vs. Non-Weekend

# Create Word List for Weekends
View(by_word)
word_list <- by_word %>% select(word, weekend_binary)
View(word_list)

word_list <- subset(word_list, word %in% bing_megabus$word)
word_list_weekend <- word_list %>%
  filter(weekend_binary==1)

# Create Word List for Weekdays
word_list_nonweekend <- word_list %>%
  filter(weekend_binary==0)
View(word_list_nonweekend)

# Word Cloud for Weekend

word_list_weekend <- Corpus(VectorSource(word_list_weekend))
inspect(word_list_weekend)

toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
word_list_weekend <- tm_map(word_list_weekend, toSpace, "/")
word_list_weekend <- tm_map(word_list_weekend, toSpace, "@")
word_list_weekend <- tm_map(word_list_weekend, toSpace, "\\|")
word_list_weekend <- tm_map(word_list_weekend, removeWords, c("megabus", "the", "and", "https", "you", "t.co", "for", "this", "bus", "that")) 

#Build a term-document matrix
dtm <- TermDocumentMatrix(word_list_weekend)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)


# Word Cloud (Weekend)
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

# Word Cloud for Weekday

word_list_nonweekend <- Corpus(VectorSource(word_list_nonweekend))
inspect(word_list_nonweekend)

toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
word_list_nonweekend <- tm_map(word_list_nonweekend, toSpace, "/")
word_list_nonweekend <- tm_map(word_list_nonweekend, toSpace, "@")
word_list_nonweekend <- tm_map(word_list_nonweekend, toSpace, "\\|")
word_list_nonweekend <- tm_map(word_list_nonweekend, removeWords, c("megabus", "the", "and", "https", "you", "t.co", "for", "this", "bus", "that")) 

#Build a term-document matrix
dtm <- TermDocumentMatrix(word_list_nonweekend)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

# Word Cloud (Weekend)
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))


####### END WORD CLOUD #######

# Word Cooccurence

View(by_word)
word_cooccurences <- by_word %>% select(word, id)
word_cooccurences <- subset(word_cooccurences, word %in% bing_megabus$word)

word_cooccurences <- word_cooccurences %>%
  pair_count(id, word, sort = TRUE)
word_cooccurences

install.packages("igraph")
if(!require(devtools)) {
  install.packages('devtools')
}
devtools::install_github('hadley/ggplot2')
devtools::install_github('thomasp85/ggforce')
devtools::install_github('thomasp85/ggraph')

library(igraph)
library(ggraph)

set.seed(2016)
word_cooccurences %>%
  filter(n >= 10) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n)) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1.8) +
  theme_void()

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
#write.csv(tweets_df, file = "megabus_tweets_df_4-26.csv")
#write.csv(by_word, file = "megabus_by_word_4-26.csv")

