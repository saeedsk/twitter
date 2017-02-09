
packages <- c("ggplot2", "lubridate", "RTextTools", "bit64", "stringr", 
              "date", "scales", "textcat", "NLP", "SnowballC", "textcat",
              "data.table", "tm.plugin.dc")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages()))) 
}

library(data.table)
library(textcat)
library(lubridate)
library(ggplot2)
library(bit64)
library(stringr)
library(date)
library(scales)
library(tm)
library(tm.plugin.dc)
library(SnowballC)
library("RTextTools") #Loads many packages useful for text mining
#----------------------------------------------------------------
printf <- function(...) cat(sprintf(...))
#----------------------------------------------------------------
load_dataset <- function() {
  #  tweets <- data.table::fread("tweets.csv", na.strings="NA", colClasses=NULL, nrows=4800000)
  tweets <- data.table::fread("tweets.csv", na.strings="NA", colClasses=NULL, nrows=4800000)
  tweets <- cbind(timestamp=as.POSIXct(paste (tweets$tweet_time, tweets$tweet_date, sep = " ") , format="%H:%M:%S %Y-%m-%d"), tweets)
  tweets <- cbind(date=as.POSIXct(tweets$tweet_date, format="%Y-%m-%d"), tweets)
  tweets$tweet_time <- NULL
  tweets$tweet_date <- NULL
  tweets
}
#----------------------------------------------------------------
remove_urls <- function(tweets)
{
  tweets$content <- gsub("?(f|ht)tp(s?)://\\S+\\s*", "", tweets$content)
  tweets
}
#----------------------------------------------------------------
remove_usernames <- function(tweets)
{
  tweets$content <- gsub("?(@)\\S+\\s*", "", tweets$content)
  tweets
}
#----------------------------------------------------------------
remove_hash_tags <- function(tweets)
{
  tweets$content <- gsub("?(#)\\S+\\s*", "", tweets$content)
  tweets
}
#----------------------------------------------------------------
remove_hash_tag_sign <- function(tweets)
{
  tweets$content <- gsub("?(#)", "", tweets$content)
  tweets
}
#----------------------------------------------------------------
remove_numbers <- function(tweets)
{
  tweets$content <- gsub("[[:digit:]]", "", tweets$content)
  tweets
}
#----------------------------------------------------------------
detect_tweets_language <- function(tweets) {
  clean.tweets <- remove_urls(tweets)
  clean.tweets <- remove_usernames(clean.tweets)
  clean.tweets <- remove_hash_tags(clean.tweets)
  clean.tweets <- remove_numbers(clean.tweets)
  
  tweets.language = textcat(clean.tweets$content)
  tweets <- cbind(language=tweets.language , tweets)
  tweets
}
#----------------------------------------------------------------
load_keywords <- function() {
  keywords <- read.csv(file="strings", header=FALSE)
  names(keywords) <- c("keyword")
  keywords
}

#----------------------------------------------------------------
find_tweet_frequency <- function(tweets) {
  
  tweets <- cbind(as.POSIXct(paste (tweets$tweet_time, tweets$tweet_date, sep = " ") , format="%H:%M:%S %Y-%m-%d"), tweets)
  tweets <- cbind(as.POSIXct(tweets$tweet_date, format="%Y-%m-%d"), tweets)
  tweets$tweet_time <- NULL
  tweets$tweet_date <- NULL
  tweets
}
#----------------------------------------------------------------
filter_dataset_by_keyword <- function( tweets, keyword )
{
  # Replace space with ".*" to build a regex expression that will search for all keywords
  keyword.regex <- str_replace_all(keyword," ",".*")
  search.result <- with(tweets , str_detect(tweets$content, keyword.regex))
  tweets[search.result,drop=TRUE]
}

#----------------------------------------------------------------
find_keyword_frequencies <- function( tweets, keywords )
{
  keywords.frequncy = list()
  for(i in 1:length(keywords$keyword)){
    keyword.regex <- str_replace_all(tolower(keywords$keyword[i])," ",".*")
    search.result <- with(tweets , str_detect(tolower(tweets$content), keyword.regex))
    keywords.frequncy[[i]] <- c( keyword = keywords$keyword[i], count = sum( unlist(search.result) ) )
    printf("Counting frequency of keyword:'%s'   \t %s\n", keywords$keyword[i], sum(unlist(search.result)))
  }
  keywords.frequncy
}

#----------------------------------------------------------------
find_frequent_terms <- function( tweets.corpus, lowfreq )
{
  tdm <- TermDocumentMatrix(tweets.corpus, control = list(wordLengths = c(3,Inf)))
  
  freq.term <- findFreqTerms(tdm, lowfreq = lowfreq)
  term.freq <- rowSums(as.matrix(tdm)) 
  term.freq <- subset(term.freq, term.freq >= lowfreq)
  df <- data.frame(term = names(term.freq), freq = term.freq)
  base <- ggplot(df, aes(x = term, y = freq)) + geom_bar(stat = "identity") + 
    xlab("Most Frequent Terms") + 
    ylab("Count") + 
    coord_flip()
  base
}
#----------------------------------------------------------------
#----------------------------------------------------------------
#----------------------------------------------------------------


#--------------- Main Code -------------------


#-------------- Loading Data -----------------

printf("Loading dataset, it might take a while ...\n")
tweets <- load_dataset() 
printf("Dataset sucessfully loaded.\n")

#-------------- Text Cleaning -----------------

tweets <- remove_urls(tweets)
tweets <- remove_usernames(tweets)
tweets <- remove_hash_tag_sign(tweets)
tweets <- remove_numbers(tweets)
#------------------ NLP -----------------------
#------------- Creating Corpus-----------------

# create a subset of tweets, since nlp engine handles datasets larger than 300K rows very slowly
tweets.sample <- tweets[sample(nrow(tweets), 100), ]

#tweets.corpus <- as.DistributedCorpus( VCorpus( VectorSource( tweets.sample$content ) ) )
tweets.corpus <- Corpus(VectorSource(tweets.sample$content))

# convert to lowercase
tweets.corpus <- tm_map(tweets.corpus, content_transformer(tolower) )

# remove punctuation
tweets.corpus <- tm_map(tweets.corpus, content_transformer(removePunctuation) )

# remove numbers
tweets.corpus <- tm_map(tweets.corpus, content_transformer(removeNumbers) )

# add three extra stop words: 'via', ...
tweets.stop.words <- c(stopwords("english"),"via","apear","will","per","get","says")

#remove stopwords from corpus
tweets.corpus <- tm_map(tweets.corpus, content_transformer(removeWords), tweets.stop.words)

View(tweets.language)
#----------- Langugae Detection ---------------
tweets <- detect_tweets_language(tweets)
View(tweets)

