
install.packages("lubridate")
install.packages("RTextTools")
library(lubridate)
library(ggplot2)
library(bit64)
library(stringr)
library("RTextTools") #Loads many packages useful for text mining

printf <- function(...) invisible(print(sprintf(...)))

load_dataset <- function() {
#  tweets <- data.table::fread("tweets.csv", na.strings="NA", colClasses=NULL, nrows=4800000)
  tweets <- data.table::fread("tweets.csv", na.strings="NA", colClasses=NULL, nrows=20)
  tweets <- cbind(timestamp=as.POSIXct(paste (tweets$tweet_time, tweets$tweet_date, sep = " ") , format="%H:%M:%S %Y-%m-%d"), tweets)
  tweets <- cbind(date=as.POSIXct(tweets$tweet_date, format="%Y-%m-%d"), tweets)
  tweets$tweet_time <- NULL
  tweets$tweet_date <- NULL
  tweets
}

load_keywords <- function() {
  keywords <- read.csv(file="strings", header=FALSE)
  names(keywords) <- c("keyword")
  keywords
}

show_histogram <- function(tweets) {
  #number of tweets per hour
  hist(tweets$timestamp, length(tweets$timestamp), breaks = 5040, freq = TRUE, xlab = "Date", ylab = " Tweets Frequency", main = " Tweets Histogram")
}


find_tweet_frequency <- function(tweets) {

  tweets <- cbind(as.POSIXct(paste (tweets$tweet_time, tweets$tweet_date, sep = " ") , format="%H:%M:%S %Y-%m-%d"), tweets)
  tweets <- cbind(as.POSIXct(tweets$tweet_date, format="%Y-%m-%d"), tweets)
  tweets$tweet_time <- NULL
  tweets$tweet_date <- NULL
  tweets
}

filter_dataset_by_keyword <- function(tweets, keyword)
{
  # Replace space with ".*" to build a regex expression that will search for all keywords
  keyword.regex <- str_replace_all(keyword," ",".*")
  search.result <- with(tweets , str_detect(tweets$content, keyword.regex))
  tweets[search.result,drop=TRUE]
}


test <- function(test.keyword)
{
  printf("Current keyword: %s\n", test.keyword)
}

generate_all_sub_datasets <- function(tweets, keywords)
{
  apply(keywords$keyword , 1, test)
  # Replace space with ".*" to build a regex expression that will search for all keywords
  keyword.regex <- str_replace_all(keyword," ",".*")
  search.result <- with(tweets , str_detect(tweets$content, keyword.regex))
  tweets[search.result,drop=TRUE]
}


tweets <- load_dataset() 
#View(tweets)
#show_histogram(tweets)

keywords <- load_keywords()
View(keywords)
dim(keywords)

generate_all_sub_datasets(tweets, keywords)


b <- filter_dataset_by_keyword(tweets,keywords$keyword[1])
View(b)
