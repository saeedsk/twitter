
install.packages("lubridate")
library(lubridate)
library(ggplot2)
library(bit64)

load_dataset <- function() {
  tweets <- data.table::fread("tweets.csv", na.strings="NA", colClasses=NULL, nrows=4000000)
  tweets <- cbind(timestamp=as.POSIXct(paste (tweets$tweet_time, tweets$tweet_date, sep = " ") , format="%H:%M:%S %Y-%m-%d"), tweets)
  tweets <- cbind(date=as.POSIXct(tweets$tweet_date, format="%Y-%m-%d"), tweets)
  tweets$tweet_time <- NULL
  tweets$tweet_date <- NULL
  tweets
}

show_histogram <- function(tweets) {
  hist(tweets$timestamp, length(tweets$timestamp), breaks = 300, freq = TRUE, xlab = "Date", ylab = " Tweets Frequency", main = " Tweets Histogram")
}


find_tweet_frequency <- function(tweets) {

  tweets <- cbind(as.POSIXct(paste (tweets$tweet_time, tweets$tweet_date, sep = " ") , format="%H:%M:%S %Y-%m-%d"), tweets)
  tweets <- cbind(as.POSIXct(tweets$tweet_date, format="%Y-%m-%d"), tweets)
  tweets$tweet_time <- NULL
  tweets$tweet_date <- NULL
  tweets
}

tweets <- load_dataset() 
#View(tweets)

show_histogram(tweets)

hist(tweets$timestamp, length(tweets$timestamp), freq = TRUE, xlab = "Date")



