require(twitteR)
require(tm)
require(wordcloud)
require(SnowballC)

setwd("")

ck <- ""
cs <- ""
at <- ""
as <- ""

setup_twitter_oauth(ck, cs, access_token = at, access_secret = as)

t_stream <- searchTwitter('', resultType="recent", n=500)

df <- do.call("rbind", lapply(t_stream, as.data.frame))

# lapply applies a function to every element of a list and returns a list as result - it transforms the
# structure into a list of data.frames (one for each tweet)
# do.call executs a function on an argument (the new tweet-list). The function is here rbind(), i.e.the elements
# are connected row-wise, creating a single data frame in the end

my_columns <- subset(df, select=c("text","created","screenName","retweetCount","isRetweet","id")) 
# we only need these columns for our analysis. there's a precision loss here in for the id

my_columns[,1] <- gsub('"',"",my_columns[,1])
# before we can use the data frame in Access we need to delete all quotation marks from the text field

write.table(my_columns, "tweets.csv", row.names = FALSE, col.names = TRUE, sep =";")
# This saves the tweets as a csv-file that can be imported into Access.

my_columns <- read.csv("tweets.csv", sep=";")

# R Task 1 Begin (Create list that has the number of characters in each tweet as elements (research function "nchar"), 
# then add the list as a column to the data frame and save the data frame to a new csv file)

characters <- list()

i<-1;
while(i<501) {
  characters[[i]] <- nchar(toString(my_columns$text[[i]]));
  i <- i+1;
}

#making list 2D otherwise dataframe export to CSV is problematic
characters <- unlist(characters, recursive = TRUE, use.names = TRUE)

my_columns$characters <- characters


write.table(my_columns, "tweetsAndLength.csv", row.names = FALSE, col.names = TRUE, sep =";")

print("task 1 done")

# R Task 1 End

# R Task 2 Begin (Create a data frame that only contains those tweets with an even ID using a FOR-loop and IF-ELSE
# constructs. Save the data frame as a new csv file)

temp_df <- df
even_odd_list <- list()

j<-1;
while(j<501) {
  # only taking last digits of ID to check if its odd for efficiency
  temp <- substr(df$id[j], nchar(df$id[j])-2, nchar(df$id[j]))
  if ( as.numeric(temp) %% 2 == 0) {
    even_odd_list[[j]] <- TRUE
  } else {
    even_odd_list[[j]] <- FALSE
  }
  j <- j+1
}

even_odd_list <- unlist(even_odd_list, recursive = TRUE, use.names = TRUE)

temp_df$even_odd <- even_odd_list

finaldf <- temp_df[temp_df$even_odd == TRUE, ]

rm(temp_df)

write.table(finaldf, "tweets_with_even_ID.csv", row.names = FALSE, col.names = TRUE, sep =";")

print("task 2 done")

# Hint (see video 2)

# R Task 2 End

# R Task 3 Begin (Create a word cloud of the contents of the tweets, save the word cloud as a png file)

#create corpus of tweets
tweetwords <- Corpus(VectorSource(df$text))
#remove invalid chars
tweetwords <- tm_map(tweetwords, function(x) iconv(enc2utf8(x), sub = "byte"))

tweetwords <- tm_map(tweetwords, tolower)

# remove subreddit mentions
removeSub <- function(x) gsub("/\\w+ *", "", x)
tweetwords <- tm_map(tweetwords, removeSub)
# remove punctuation 
tweetwords <- tm_map(tweetwords, removePunctuation) 
# remove numbers 
tweetwords <- tm_map(tweetwords, removeNumbers) 
# remove URLs 
removeURL <- function(x) gsub("http[[:alnum:]]*", "", x) 
tweetwords <- tm_map(tweetwords, removeURL) 
 
# add via to stopwords
myStopwords <- c(stopwords("english"), "via") 
# remove stopwords from corpus 
tweetwords <- tm_map(tweetwords, removeWords, myStopwords)

wordcloud(tweetwords, scale=c(5,0.5), max.words=100, random.order=FALSE, rot.per=0.35, use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))

print("task 3 done")
# R Task 3 End
