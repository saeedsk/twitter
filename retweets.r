

packages <- c("ggplot2", "lubridate", "RTextTools", "bit64", "stringr", 
              "date", "scales", "textcat", "NLP", "SnowballC", "textcat",
              "data.table", "tm.plugin.dc", "plyr", "dplyr", "forcats")
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
library("plyr")
library("dplyr")
library(forcats) #for retweet
# load tweets
# tweets <- data.table::fread("tweets.csv", na.strings="NA", colClasses=NULL, nrows=4800000)
  tweets <- data.table::fread("tweets.csv", na.strings="NA", colClasses=NULL, nrows=480)
  tweets <- cbind(timestamp=as.POSIXct(paste (tweets$tweet_time, tweets$tweet_date, sep = " ") , format="%H:%M:%S %Y-%m-%d"), tweets)
  tweets <- cbind(date=as.POSIXct(tweets$tweet_date, format="%Y-%m-%d"), tweets)
  tweets$tweet_time <- NULL
  tweets$tweet_date <- NULL
  tweets
#load keywords
keywords <- read.csv(file="strings", header=FALSE)
  names(keywords) <- c("keyword")
  
  # Get RTs, senders, receivers
rts <- grep("^rt @[a-z0-9_]{1,15}", tolower(tweets$content), perl=T, value=T);
rt.sender <- tolower(as.character(tweets$username[grep("^rt @[a-z0-9_]{1,15}", tolower(tweets$content), perl=T)]));
rt.receiver <- gsub("^rt @([a-z0-9_]{1,15})[^a-z0-9_]+.*$", "\\1", rts, perl=T);
rt.content <- gsub("^rt", "\\1", rts, perl=T);

print(paste(length(rts), " RTs from ", length(unique(rt.sender)), " senders and ", length(unique(rt.receiver)), " receivers.",sep=""));

# This is necessary to avoid problems with empty entries, usually caused by encoding issues in the source files
rt.sender[rt.sender==""] <- "<NA>";
rt.receiver[rt.receiver==""] <- "<NA>";
rt.content[rt.content==""] <- "<NA>";
# Create a data frame from the sender-receiver information
rts.df <- data.frame(rt.sender, rt.receiver, rt.content);

#frequency of being retweeted
rt.receiver.freq <- count(rts.df,rt.receiver)
#extract the top 10 being retweeted, can be changed
rt.receiver.freq.top <- top_n(rt.receiver.freq,10,n)

#frequency of retweeter
rt.sender.freq <- count(rts.df,rt.sender)

#extract the top 10 being retweeted, can be changed
rt.sender.freq.top <- top_n(rt.sender.freq,10,n)

#plot receiver
ggplot(rt.receiver.freq.top, aes(reorder(rt.receiver,n), y = n)) + theme_bw() + geom_bar(stat = "identity")+ 
    xlab("Being Retweeted the Most") + 
    ylab("Count") + coord_flip()

ggsave("Being Retweeted the Most.png")
#plot sender
ggplot(rt.sender.freq.top, aes(reorder(rt.sender,n), y = n)) + theme_bw() + geom_bar(stat = "identity")+ 
    xlab("Retweeted the Most") + 
    ylab("Count") + coord_flip()
ggsave("Retweeted the Most.png")