
packages <- c("ggplot2", "lubridate", "RTextTools", "bit64", "stringr", 
              "date", "scales", "textcat", "NLP", "SnowballC", "textcat",
              "data.table", "tm.plugin.dc", "igraph")
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
library("RTextTools")
library(igraph)
#Loads many packages useful for text mining
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
remove_urls <- function(tweets){
  tweets$content <- gsub("?(f|ht)tp(s?)://\\S+\\s*", "", tweets$content)
  tweets
}

get_rt<- function(){
# Get RTs, senders, receivers
	rts <- grep("^rt @[a-z0-9_]{1,15}", tolower(tweets$content), perl=T, value=T);
	rt.sender <- tolower(as.character(tweets$username[grep("^rt @[a-z0-9_]{1,15}", tolower(tweets$content), perl=T)]));
	rt.receiver <- gsub("^rt @([a-z0-9_]{1,15})[^a-z0-9_]+.*$", "\\1", rts, perl=T);
	print(paste(length(rts), " RTs from ", length(unique(rt.sender)), " senders and ", length(unique(rt.receiver)), " receivers.", sep=""));
	# This is necessary to avoid problems with empty entries, usually caused by encoding issues in the source files
	rt.sender[rt.sender==""] <- "<NA>";
	rt.receiver[rt.receiver==""] <- "<NA>";
}
#----------------------------------------------------------------
# Create a data frame from the sender-receiver information
rts.df <- data.frame(rt.sender, rt.receiver);
#----------------------------------------------

ggplot(rts.df, aes(x=rt.receiver)) +
    geom_bar(fill = "midnightblue") + 
    theme(legend.position="none", axis.title.x = element_blank()) +
    ylab("Number of tweets") + 
    ggtitle("Retweets")  + coord_flip()