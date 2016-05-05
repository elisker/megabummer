library(tm)
library(wordcloud)
library(memoise)
library(readr)

options(digits = 22) # to prevent tweet id from truncating
q1_2015 <- read_csv("2015-12-01.csv")
names(q1_2015) <- c("id","username","text","date","geo","retweets","favorites","mentions","hashtags")
q2_2015 <- read_csv("2015-q2.csv")
names(q2_2015) <- c("id","username","text","date","geo","retweets","favorites","mentions","hashtags")
q1_2015 <- q1_2015[10,]
q2_2015 <- q2_2015[10,]
# The list of valid books
books <<- list("Q1 2015" = q1_2015)

# Using "memoise" to automatically cache the results
getTermMatrix <- memoise(function(book) {
  # Careful not to let just any name slip in here; a
  # malicious user could manipulate this value.
  if (!(book %in% books))
    stop("Unknown book")
  
  text <- readLines(sprintf("2015-12-01.csv",book),
                    encoding="UTF-8")
  
  myCorpus = Corpus(VectorSource(text))
  myCorpus = tm_map(myCorpus, content_transformer(tolower))
  myCorpus = tm_map(myCorpus, removePunctuation)
  myCorpus = tm_map(myCorpus, removeNumbers)
  myCorpus = tm_map(myCorpus, removeWords,
                    c(stopwords("SMART"), "thy", "thou", "thee", "the", "and", "but"))
  
  myDTM = TermDocumentMatrix(myCorpus,
                             control = list(minWordLength = 1))
  
  m = as.matrix(myDTM)
  
  sort(rowSums(m), decreasing = TRUE)
})