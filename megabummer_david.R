#Documentation for twitteR: https://cran.r-project.org/web/packages/twitteR/twitteR.pdf

library(dplyr)
library(ggplot2)
library(ggthemes)
library(readr)

#### DATA CLEANING AND EXPLORATORY ANALYSIS ####

# load older tweets and merge datasets
options(digits = 22) # to prevent tweet id from truncating

#full data set
tweets_df_all <- read_csv("Jan_2015-April_2016.csv")
#get a subset of random lines from this to work with:
#tweets_df_all <- tweets_df_all[sample(1:nrow(tweets_df_all), 10000, replace=FALSE),]


names(tweets_df_all) <- c("id","username","text","date","geo","retweets","favorites","mentions","hashtags")

tweets_df_all <- tweets_df_all %>%
  mutate(time=format(date, format="%H:%M:%S")) %>%
  mutate(date2 = format(date, format="%m-%d-%y")) %>%
  mutate(month = months(as.Date(date2,'%m-%d-%y'))) %>%
  mutate(weekend = weekdays(as.Date(date2,'%m-%d-%y'))) %>%
  mutate(weekend_binary = ifelse(weekend == "Saturday"|weekend=="Sunday"|weekend=="Friday", 1, 0)) %>% 
  filter(date2 != "12-31-14") %>%
  filter(date < "2016-04-01")

#ggplot(tweets_df_all, aes(unclass(date))[1]) + geom_density()

# filter out duplicates
tweets_df_all <- tweets_df_all %>%
  distinct(id)
#nrow(tweets_df_all)
tweets_df_all <- tweets_df_all %>%
  distinct(text)

# explore number of tweets per user including megabus handles
prolific_tweeters_all <- tweets_df_all %>% 
  group_by(username) %>%
  summarise(tweets = n()) %>%
  arrange(desc(tweets)) 

# filter out tweets from megabus operators
tweets_df_all = tweets_df_all[!grepl("megabus|megabusuk|MegabusHelp|megabusit|megabusde|megabusGold", tweets_df_all$username),]

# explore number of tweets per user excluding megabus handles
prolific_tweeters_filtered <- tweets_df_all %>% 
  group_by(username) %>%
  summarise(tweets = n()) %>%
  arrange(desc(tweets))

# Histogram of number of tweets
ggplot(filter(prolific_tweeters_filtered, tweets>0), aes(tweets)) + 
  geom_histogram(binwidth = 1) + 
  xlab("Number of megabus tweets per user") + 
  ylab("Frequency") + 
  theme_hc()

#Tweets per day
ggplot(data=tweets_df_all, aes(x=as.Date(date2,'%m-%d-%y'))) + 
  geom_histogram(aes(fill=..count..), binwidth=1) + 
  scale_x_date("Date") + 
  scale_y_continuous("Frequency")

#The outlying days with high tweet volume are:
tweets_df_all %>% group_by(date2) %>% count(date2, sort = TRUE) %>% filter(n>500)
#The outlying days with low tweet volume are:
tweets_df_all %>% group_by(date2) %>% count(date2, sort = TRUE) %>% filter(n<100)

#TODO (Emily) a version of the above chart that is smooth
#TODO (Ali) include the articles in the Rmd, do what Rafa does


###### SENTIMENT ANALYSIS ######

library(devtools)
#install_github("juliasilge/tidytext")
library(tidytext)
# other text mining: tm, quanteda

library(tidyr)
library(readr)

by_word <- tweets_df_all %>%
  dplyr::select(text, id, date, date2, time, weekend, weekend_binary, month) %>%
  unnest_tokens(word, text) 

# look at most commonly tweeted words
by_word_count <- by_word %>%
  count(word, sort = TRUE) 
head(by_word_count)

megabus_lexicon <- read_csv("megabus_lexicon.csv")


# create new dataframe of bing and megabummer sentiments
bing_megabus <- megabus_lexicon %>%
  filter(lexicon %in% c("bing","megabummer")) %>%
  dplyr::select(-score)
head(bing_megabus %>% filter(lexicon=="megabummer"))


# join tweets with sentiment and add score column
mb_sentiment <- by_word %>%
  inner_join(bing_megabus) %>%
  mutate(score = ifelse(sentiment == "positive", 1, -1))
head(mb_sentiment %>% dplyr::select(id,word,sentiment,score))

# calculate score for each tweet
library(data.table)
dt <- data.table(mb_sentiment)


mb_sentiment_tweet <- unique(dt[,list(score_tweet = sum(score), freq = .N, date, weekend_binary, date2, weekend, month), by = c("id")] )
tweets_df_all_joiner <- tweets_df_all %>% dplyr::select(id,text)
mb_sentiment_tweet <- left_join(mb_sentiment_tweet,data.table(tweets_df_all_joiner),by="id")
head(mb_sentiment_tweet)

#Creating data table of calendar dates, including weekend status, day of week (column name weekend), month, and tweet frequency and sentiment
mb_sentiment_date <- unique(mb_sentiment_tweet[,list(score_date = round(mean(score_tweet),2), freq = .N, weekend_binary, weekend, month), by = c("date2")] )
mb_sentiment_date <- mb_sentiment_date %>% filter(freq<500)
head(mb_sentiment_date)

#Creating data table of calendar dates and tweet frequency and sentiment with holiday status
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
head(mb_sentiment_holidays)

#TODO probably delete this line #mb_sentiment_day <- unique(mb_sentiment_tweet[,list(score_date = round(mean(score_tweet),2), freq = .N), by = c("weekend")] )

# summary stats
library(Hmisc)

#Exploratory Data Analysis
#TODO optionally (Leo) make this less redundant
describe(mb_sentiment_tweet)
describe(mb_sentiment_date)
describe(mb_sentiment_holidays)

# As you can see, the line graph of the sentiment score over time is not that useful. 
ggplot(data=mb_sentiment_tweet, aes(x=date, y=score_tweet)) + 
  geom_line()

# Instead of fitting a line, we can stratify by date and compute the mean sentiment score,referred to as _bin smoothing_. 
# Smoothing is useful for this analysis as it is designed to estimate $f(x)$ when the shape is unknown, but assumed to be _smooth_.  
# When we group the data points into strata, days in this case, that are expected to have similar sentiments and calculate the average
# or fit a simple model in each strata. We assume the curve is approximately constant within the bin  and that all the sentiment scores 
# in that bin have the same expected value. 
ggplot(data=mb_sentiment_tweet, aes(x=date, y=score_tweet)) + 
  geom_smooth()

#TODO: put the smoothed version of frequency by date right here to compare it to the tweet sentiment over time
#looking at volume in the same way:

#TODO (Emily): Plz smooth this
#TODO if we can get around to it: show it like a bell curve and indicate standard deviation lines
# histogram of all sentiment scores (tweet)
ggplot(data=mb_sentiment_tweet, aes(score_tweet)) + 
  geom_histogram(binwidth = 1)
#Notice that there are few tweets with sentiment score zero. This indicates a bimodal distribution.
#This makes sense because why would you tweet a neutral tweet.

# histogram of all sentiment scores (day)
ggplot(data=mb_sentiment_date, aes(score_date)) + 
  geom_histogram(binwidth = 0.1)

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


#Hyp. #0: sentiment ~ day of the week when stratifying on freq.
fit <- lm(score_date ~ weekend + freq, data=mb_sentiment_date)
summary(fit) # show results

#REJECT THE NULL, conclusion: The volume of tweets on a given day is a statistically 
# significant predictor of the average daily sentiment score. For every additional tweet, 
# we would expect a 0.001 decrease in average daily sentiment score. The days of the week are 
# not significantly associated with the average daily sentiment score. 

#Hyp. #0a: sentiment ~ month when stratifying on freq.
fit <- lm(score_date ~ month + freq, data=mb_sentiment_date)
summary(fit) # show results

#REJECT THE NULL, conclusion: When we stratify on volume of tweets, we see that month is a statistically 
# significant predictor of the average daily sentiment score in some cases. The months of December, June,
# and October are statistically significantly associated with average daily sentiment score. Looking at the month of
# December, for example, December we would expect, on average, a 0.127 decrease in average daily sentiment for 
# every additional tweet.

#Hyp. #1: tweet sentiment on low volume days = on high volume days
h1.lm <- lm(score_date ~ freq, data = mb_sentiment_date)
summary(h1.lm)
#REJECT THE NULL (STRONGLY), conclude that tweet sentiment on low volume days > tweet sentiment on high volume days
# The volume of tweets on a given day is a statistically significant predictor of the average daily sentiment score and
# that for every additional tweet, we would expect a 0.001 decrease in average daily sentiment score.

# graph tweet sentiment as a function of tweet volume
#echart
ggplot(data=mb_sentiment_date, aes(x=freq, y=score_date)) + 
  geom_line() + xlab("Number of megabus tweets") + 
  ylab("Tweet sentiment score") + 
  ggtitle("Tweet sentiment as a function of tweet volume") + 
  theme_hc()
#Commentary:
#We have found a highly statistically significant trend.
#It is a huge data set and the association 
#is not strong (small magnitude, a lot of variance).
#In order to explore the meaning and practical application of the relationship,
#we would need to further examine days with different sentiments of tweets and 
#consider whether the difference as measured by our scoring system is meaningful.

score_date.res = resid(h1.lm)
plot(mb_sentiment_date$freq, score_date.res, 
ylab="Residuals", xlab="Number of Tweets", 
main="Tweets and Megabus Sentiment")
abline(0,0)
#There is homoscadesticity of residuals, therefore linear regression is
#an appropriate method of analysis.
                

# Evaluate homoscedasticity
# non-constant error variance test
#install.packages('car')
library(car)
ncvTest(h1.lm)
# We fail to reject the null hypothesis of homoskedastic errors

# Normality of Residuals
# As demonstrated in the QQ-Plot below of the studendized residuals, the
# linearity of the points suggests that the residuals are normally distributed and further
# confirms that linear regression is appropriate for our dataset.
qqPlot(h1.lm, main="QQ Plot")

# distribution of studentized residuals
# We can also use a combination of a density plot and histogram to visualize that the
# normality assumption holds true as demonstrated below.
library(MASS)
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
#Conclusion: When looking at all weekend vs weekday tweet scores as one large group, the
#mean tweet score on weekends is not significantly different than the mean tweet score on non-weekends, 
#*without* stratifying on tweet volume.
#echart
ggplot(mb_sentiment_tweet, aes(x=weekend_binary, y=score_tweet, group=weekend_binary)) +
  geom_boxplot(aes(fill=weekend_binary)) +
  xlab("Non-weekend vs weekend") + 
  ylab("Tweet sentiment score") + 
  ggtitle("Variation in tweet sentiment between weekends and non-weekends") + 
  geom_jitter(colour="gray40",
              position=position_jitter(width=0.2), alpha=0.3) 

#Hyp. #3: sentiment weekend = sentiment weekday 
#(looking at weekend and nonweekend as two large groups of DAILY AVERAGES
#of tweet scores)
the_weekend_date = mb_sentiment_date %>% filter(weekend_binary == 1)
not_the_weekend_date = mb_sentiment_date %>% filter(weekend_binary == 0)
var.test(the_weekend_date$score_date,not_the_weekend_date$score_date)#variances are equal if p-value > 0.05
t.test(the_weekend_date$score_date,not_the_weekend_date$score_date)#,var.equal = TRUE)
#Conclusion: The mean of daily tweet scores on weekends is significantly less than the
#mean tweet score on non-weekends, *without* stratifying on tweet volume.
#echart
ggplot(mb_sentiment_date, aes(x=weekend_binary, y=score_date, group=weekend_binary)) +
  geom_boxplot(aes(fill=weekend_binary)) +
  xlab("Non-weekend vs. weekend") + 
  ylab("Tweet sentiment score") + 
  ggtitle("Variation in tweet sentiment between weekends and non-weekends") + 
  geom_jitter(colour="gray40",
              position=position_jitter(width=0.2), alpha=0.3) 

#Hyp. #4: sentiment weekend = sentiment weekday when stratifying on freq.
#[multiple linear regression]
fit <- lm(score_date ~ weekend_binary + freq, data=mb_sentiment_date)
summary(fit) # show results
#DO NOT HAVE ENOUGH EVIDENCE TO REJECT THE NULL, conclusion:  The weekend is not a statistically signiificant predictor 
# oF average daily sentiment score when we stratify on tweet volume.

#Hyp. #5: tweet volume weekend = tweet volume weekday

var.test(the_weekend_date$freq,not_the_weekend_date$freq)
t.test(the_weekend_date$freq,not_the_weekend_date$freq)

#REJECT THE NULL (STRONGLY), conclude that tweet volume on weekends is statistically differrent than 
# tweet volume on weekdays. 
#echart
ggplot(mb_sentiment_date, aes(x=weekend_binary, y=freq, group=weekend_binary)) +
  geom_boxplot(aes(fill=weekend_binary)) +
  xlab("Non-weekend vs. weekend") + 
  ylab("Tweet volume") + 
  ggtitle("Variation in tweet volume between weekends and non-weekends") + 
  geom_jitter(colour="gray40",
              position=position_jitter(width=0.2), alpha=0.3) 
#TODO (Leo) fix weekend_binary scale so that it makes sense, hide the x axis, legend with two colors

#Hyp. #6: sentiment holiday = sentiment !holiday
#Without stratifying on volume
holiday = mb_sentiment_holidays %>% filter(holiday == 1)
not_holiday = mb_sentiment_holidays %>% filter(holiday == 0)
var.test(holiday$score_date,not_holiday$score_date)#variances are equal if p-value > 0.05
t.test(holiday$score_date,not_holiday$score_date,var.equal = TRUE)
#There is no statistically significant association between holiday and tweet sentiment when we do not stratify on volume.

#Stratifying on volume
#[multiple linear regression]
fit <- lm(score_date ~ holiday + freq, data=mb_sentiment_holidays)
summary(fit) # show results
#When we stratify on tweet volume, we still see that there is no statistically significant association between holiday and tweet sentiment.
#echart
ggplot(mb_sentiment_holidays, aes(x=holiday, y=score_date, group=holiday)) +
  geom_boxplot(aes(fill=holiday)) +
  xlab("Non-holiday vs. holiday") + 
  ylab("Tweet sentiment score") + 
  ggtitle("Variation in tweet sentiment score between holidays and non-holidays") + 
  geom_jitter(colour="gray40",
              position=position_jitter(width=0.2), alpha=0.3) 



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

positives <- bing_megabus %>%
  filter(sentiment == "positive") %>%
  dplyr::select(word)

negatives = bing_megabus %>%
  filter(sentiment == "negative") %>%
  dplyr::select(word)

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

head(by_word)
word_list <- by_word %>% dplyr::select(word)
head(word_list)
word_list_negatives <- subset(word_list, word %in% negatives$word)
head(word_list_negatives)

word_list_positives <- subset(word_list, word %in% positives$word)
head(word_list_positives)

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
#inspect(word_list_positives)

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
head(by_word)
word_list <- by_word %>% dplyr::select(word, weekend_binary)
head(word_list)

word_list <- subset(word_list, word %in% bing_megabus$word)
word_list_weekend <- word_list %>%
  filter(weekend_binary==1)

# Create Word List for Weekdays
word_list_nonweekend <- word_list %>%
  filter(weekend_binary==0)
head(word_list_nonweekend)

# Word Cloud for Weekend

word_list_weekend <- Corpus(VectorSource(word_list_weekend))
#inspect(word_list_weekend)

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

word_cooccurences <- by_word %>% dplyr::select(word, id)
word_cooccurences <- subset(word_cooccurences, word %in% bing_megabus$word)

word_cooccurences <- word_cooccurences %>%
  pair_count(id, word, sort = TRUE) %>%
  dplyr::filter(n>25)
word_cooccurences

#if(!require(devtools)) {
#  install.packages('devtools')
#}
library(devtools)
#is it necessary to run the below line if library(ggplot2) has already been called? -Leo
#devtools::install_github('hadley/ggplot2')
#devtools::install_github('thomasp85/ggforce')
#devtools::install_github('thomasp85/ggraph')

library(igraph)
library(ggraph)

set.seed(2016)
word_cooccurences %>%
  filter(n >= 10) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour="gray") +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1.8, size=5) +
  theme_void()


#####END LEO'S EDITS 2:45PM 5/4#####

library(gapminder)

theme_set(theme_bw())
#library(copula)

m <- as.matrix(word_list)
# calculate the frequency of words and sort it by frequency
word.freq <- sort(rowSums(m), decreasing = T)
# colors
pal <- brewer.pal(9, "BuGn")[-(1:4)]

#m <- as.matrix(tdm)
# calculate the frequency of words and sort it by frequency
word.freq <- sort(rowSums(m), by=word, decreasing = T)
# colors
pal <- brewer.pal(9, "BuGn")[-(1:4)]
#plotting the wordcloud
wordcloud(words = names(word.freq), freq = word.freq, min.freq = 3,
          random.order = F, colors = pal)

# Export file as csv
#write.csv(tweets_df, file = "megabus_tweets_df_4-26.csv")
#write.csv(by_word, file = "megabus_by_word_4-26.csv")

head(by_word)
word_list <- by_word %>% dplyr::select(word, date2)
head(word_list)

word_list <- subset(word_list, word %in% bing_megabus$word)
word_list_date <- word_list %>%
  filter(date2=="01-01-15")
word_list_date <- word_list_date %>% dplyr::select(word)

word_list_date <- Corpus(VectorSource(word_list_date))
#inspect(word_list_date)

toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
word_list_date <- tm_map(word_list_date, toSpace, "/")
word_list_date <- tm_map(word_list_date, toSpace, "@")
word_list_date <- tm_map(word_list_date, toSpace, "\\|")
word_list_date <- tm_map(word_list_date, removeWords, c("megabus", "the", "and", "https", "you", "t.co", "for", "this", "bus", "that")) 

#Build a term-document matrix
dtm <- TermDocumentMatrix(word_list_date)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)


# Word Cloud (Date)
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

