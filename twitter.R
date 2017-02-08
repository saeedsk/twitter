
packages <- c("ggplot2", "lubridate", "RTextTools", "bit64", "stringr", "date", "scales", "textcat", "NLP", "SnowballC")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages()))) 
}

library(textcat)
library(lubridate)
library(ggplot2)
library(bit64)
library(stringr)
library(date)
library(scales)
library(tm)
library(SnowballC)
library("RTextTools") #Loads many packages useful for text mining
#----------------------------------------------------------------
printf <- function(...) cat(sprintf(...))
#----------------------------------------------------------------
load_dataset <- function() {
#  tweets <- data.table::fread("tweets.csv", na.strings="NA", colClasses=NULL, nrows=4800000)
  tweets <- data.table::fread("tweets.csv", na.strings="NA", colClasses=NULL, nrows=480)
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
prepare_histogram <- function(tweets, keyword) {
  #number of tweets per hour
  #hist(tweets$timestamp, length(tweets$timestamp), breaks = 5040, freq = TRUE, xlab = "Date", ylab = "Tweets Frequency", main = paste("Keyword:'",keyword,"'"))
  lims <- as.Date(strptime(c("2016-01-01 00:00","2016-07-31 23:59"), format = "%Y-%m-%d %H:%M"))
  dat <- data.frame(tweets$timestamp)
  dat$tweets.timestamp <- as.Date(dat$tweets.timestamp, format="%d/%m/%Y")
  base <- ggplot(data=dat, aes(dat$tweets.timestamp))+
    geom_histogram( aes(dat$tweets.timestamp), 
                    col="black", fill="black", binwidth = 0.5,alpha = 0.6)+
    theme_bw()+
    ggtitle( paste("Keyword: '",keyword,"'") )+
    theme(plot.title = element_text(hjust = 0.5))+
    xlab("Date of tweets")+
    ylab("Number of tweets") +
    scale_x_date(limits=lims,breaks=date_breaks("15 days"),
                 labels=date_format("%b %d %y") )
  base
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
tweets <- remove_hash_tags(tweets)
tweets <- remove_numbers(tweets)
#------------- Creating Corpus-----------------
tweets.corpus <- Corpus(VectorSource(tweets$content))
# convert to lowercase
tweets.corpus <- tm_map(tweets.corpus, tolower)
# remove punctuation
tweets.corpus <- tm_map(tweets.corpus, removePunctuation)
# remove numbers
tweets.corpus <- tm_map(tweets.corpus, removeNumbers)
# stem words
tweets.corpus <- tm_map(tweets.corpus, stemDocument)




#----------- Langugae Detection ---------------
tweets <- detect_tweets_language(tweets)
View(tweets)
#------- Frequent Words and Associations ------


#--------------- Word Cloud -------------------

#--------------- Clustering -------------------

printf("Loading keywords ...\n")
keywords <- load_keywords()
keywords

printf("Filtering the tweets dataset based on the provided keywords...\n")
segmented.tweets.list = list()
for(i in 1:length(keywords$keyword)){
  printf("Preparing dataset %d base on '%s' keyword\n",i , keywords$keyword[i])
  segmented.tweets.list[[i]] <- filter_dataset_by_keyword( tweets, keywords$keyword[i] )
}

#----------- Cluster Histogram ---------------

histogram.list = list()
for(i in 1:length(keywords$keyword)){
  printf("Generating Histogram %d \n",i)
  if ( nrow(segmented.tweets.list[[i]] ) > 0)
  {
     histogram.list[[i]] <- prepare_histogram(segmented.tweets.list[[i]], keywords$keyword[i])
     printf("Saving Histogram Image %d \n",i)
     histogram.filename = paste("plots\\histogram-",i,".png")
     if (length(histogram.list[[i]]) > 0)
       ggsave(histogram.list[[i]], file=histogram.filename, width=16, height=9);
  }
  else
  {
    printf("Histogram %d is empty\n",i)
  }
}
#------------- Topic Modeling -----------------

View(tweets)


#sapply(X = keywords$keyword , FUN = filter_dataset_by_keyword, tweets = tweets)

