###########################################
# Authors: Anna Quaglieri & Saskia Freytag
# Date: 14th May 2017
# Rladies Melbourne twitter workshop 
###########################################

########################
# Set working directory
########################

dir <- "./R-Ladies-master"

###############
# Load packages
###############

list.of.packages <- c("twitteR","ROAuth","RCurl","RJSONIO","RSentiment","tm","wordcloud","tidyr","dplyr","ggplot2","plotly")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

lapply(list.of.packages, require, character.only = TRUE)

##############################
# Load data previously created
##############################

combineTweets <- read.csv(file.path(dir,"Data","tweets_hastags_combined.csv"),stringsAsFactors = FALSE)

#################################
# Plot the number of daily tweets
#################################

toDF_rupol_daily <- combineTweets %>% 
  tidyr::separate(created, into = c("Day","Time"), sep = " ") %>%
	dplyr::group_by(Day) %>% 
	dplyr::summarise(tweetsPerDay = length(text)) %>%
	ggplot(aes(x = Day, y = tweetsPerDay)) + geom_bar(stat = "identity") + 
	theme(axis.text.x = element_text(angle = 45, hjust = 1)) + coord_flip()
toDF_rupol_daily

############################
# Lookup user UD information
############################

head(combineTweets[,c("id","screenName")])
# Create list of screenNames to look up for
screen_names <- unique(as.character(combineTweets$screenName))

# Since the number of users to lookup is larger than the maximum allowed in one go
# we need to split up the screen_names object and do the search several times. For the 
# sake of time the final dataset with user's information has already been created and merged 
# with the tweets data. You can find information on how to create it in the twitteR.Rmd

## Load the combine data set with tweets and user's info
combine_data <- read.csv(file.path(dir,"Data","Users_infos_and_tweets.csv"), stringsAsFactors = FALSE)
head(combine_data)

## Some analysis

# 1. How many times is a drag queen mentioned daily?

# Queen and twitter names of the queens
queens <- read.csv(file.path(dir,"Data","queens_name.csv"))
head(queens)

# Separate their queen name into their components
queens <- queens %>% 
  tidyr::separate(Queen.Name, into = c("Queen1","Queen2","Queen3"), sep = " ", remove = FALSE)
head(queens[,c("Queen1","Queen2","Queen3")])

# Wrapper function that creates a vector of key names for every queen
queen_vector <- function(x){
	vec <- c(x[c("Queen1","Queen2","Queen3","Twitter.Name")])
	vec <- vec[!is.na(vec)]
}

# List containing the vectors for every queen
queens_vecs <- apply(queens, 1,queen_vector)
queens_grepKey_prepare <- lapply(queens_vecs, function(x) paste0(x, collapse = "|"))

# Set the encoding of the tweets as latin to avoid issues with for example emoji
Encoding(combine_data$text) <- "latin1"
grep_queens <- lapply(queens_grepKey_prepare, function(x) grep(x,combine_data$text))
names(grep_queens) <- queens$Twitter.Name
# Index referring to the raw in combine_data where a queen was mentioned
head(grep_queens[[1]])

# Frequency of tweets per queen
# 1. Exctract rows where a queen was mentioned and extract only columns that we need for this analysis
freq_mention_Day <- lapply(grep_queens, function(x){
	mention_data <- combine_data[x,c("Day","Time","text","location","followersCount","friendsCount","retweetCount", "isRetweet", "retweeted")]
})
# 2. Combine mention for every queen into a data.frame 
freq_mention_DayToDF <- do.call(rbind,freq_mention_Day)
# 3. Creat a column $queen_name which will tell us whose queen the tweet belongs to
number_mention <- sapply(freq_mention_Day,function(x) nrow(x))
freq_mention_DayToDF$queen_name <- rep(names(freq_mention_Day), times = number_mention)

# Plot the number of times that a queen was mentioned in a tweet daily
dailyMention <- freq_mention_DayToDF %>% dplyr::group_by(Day, queen_name) %>% 
  dplyr::summarise(Nmention = length(text)) %>%
  ggplot(aes(x = Day, y = Nmention, colour = queen_name, group = queen_name)) + 
  geom_line() + theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  geom_vline(xintercept= c(4,11,18), linetype = "dotted")
dailyMention
plotly::ggplotly(dailyMention) 

# Episodes in America
# Airdate: April 28, 2017 (29th in AU)
# Airdate: May 5th 2017 (6th in AU)
# Airdate: May 12th 2017 (13th in AU)

# 2. How come @atlsexyxlim has such huge number of tweets?
# Let's look at the retweets

retweets <- freq_mention_DayToDF %>% dplyr::group_by(Day, queen_name) %>%
  dplyr::summarise(NUniqueTweet = length(unique(text)), Nretweet = sum(isRetweet)) %>%
  ggplot(aes(x = NUniqueTweet, y = Nretweet, colour = queen_name)) + geom_point() + theme_bw()
retweets
plotly::ggplotly(retweets)

atlslim <- subset(freq_mention_DayToDF, queen_name %in% "@atlsexyslim")

# 3. word cloud of @atlsexyslim tweets
some_txt <- freq_mention_DayToDF$text[freq_mention_DayToDF$queen_name=="@atlsexyslim"]
# Clean text
# remove punctuation
some_txt = gsub("[[:punct:]]", "", some_txt)
# remove numbers
some_txt = gsub("[[:digit:]]", "", some_txt)
# remove html links
some_txt = gsub("http\\w+", "", some_txt)
# remove unnecessary spaces
some_txt = gsub("[ \t]{2,}", "", some_txt)
some_txt = gsub("^\\s+|\\s+$", "", some_txt)

mach_corpus = tm::Corpus(tm::VectorSource(some_txt))

tdm = tm::TermDocumentMatrix(mach_corpus,
                    control = list(removePunctuation = TRUE,
        stopwords = c("machine", "learning", tm::stopwords("english")),
        removeNumbers = TRUE, tolower = TRUE))   
   
# define tdm as matrix
dm = as.matrix(tdm)
# get word counts in decreasing order
word_freqs = sort(rowSums(dm), decreasing=TRUE) 
# create a data frame with words and their frequencies
dm = data.frame(word=names(word_freqs), freq = word_freqs)

# Remove expected high frequency words 
head(dm)
dm <- dm[-(1:5),]
# Plot the word cloud
wordcloud::wordcloud(dm$word, dm$freq, random.order=FALSE, max.words = 100, colors=brewer.pal(8, "Dark2"))

## 4. Sentiment 
# Extract tweets and store them into a character vector with a unique ID
freq_mention_DayToDF$uniqueID <- rownames(freq_mention_DayToDF)
some_txt <- freq_mention_DayToDF$text
names(some_txt) <- freq_mention_DayToDF$uniqueID

# Clean the text
# remove retweet entities
some_txt = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", some_txt)
# remove at people
some_txt = gsub("@\\w+", "", some_txt)
# remove punctuation
some_txt = gsub("[[:punct:]]", "", some_txt)
# remove numbers
some_txt = gsub("[[:digit:]]", "", some_txt)
# remove html links
some_txt = gsub("http\\w+", "", some_txt)
# remove unnecessary spaces
some_txt = gsub("[ \t]{2,}", "", some_txt)
some_txt = gsub("^\\s+|\\s+$", "", some_txt)

# convert every word to lower case
# define "tolower error handling" function 
try.error = function(x)
{
  # create missing value
  y = NA
  # tryCatch error
  try_error = tryCatch(tolower(x), error=function(e) e)
  # if not an error
  if (!inherits(try_error, "error"))
    y = tolower(x)
  # result
  return(y)
}
# lower case using try.error with sapply 
some_txt = sapply(some_txt, try.error)

# remove NAs in some_txt
some_txt = some_txt[!is.na(some_txt)]
names(some_txt) = NULL
some_txt <- gsub("\n","",some_txt)

# Run sentiment analysis and plot results
emotion <- RSentiment::calculate_sentiment(some_txt)
emotion$uniqueID <- names(some_txt)
combine_sentiment <- cbind(freq_mention_DayToDF,emotion)
 
ggplot(combine_sentiment, aes(x = queen_name, fill = sentiment)) +
geom_bar(stat = "count", position = "fill") + coord_flip()

# 5. Which other question can be asked?


