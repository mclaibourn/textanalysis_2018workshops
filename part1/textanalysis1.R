###################################################################################
# TEXT ANALYSIS WITH R, PART I
# 1. Acquire text
# 2. Read text into R: readtext
# 3. Clean up text data
# 4. Create corpus, assign docvars
# 5. Explore corpus and metadata: metadata as dataframe, kwic, readability
# 6. Process and prepare text: tokenize (bag-of-words), ngrams/phrases, stopwords
# 7. Describe text data from a dfm: dfm, stemming, frequencies, lexical diversity
# 8. Compare text data: grouping, relative frequency, keyness
# Michele Claibourn (mclaibourn@virginia.edu)
# Created: October 12, 2018
# Updated: October 16, 2018
###################################################################################

# 0. set up your workspace -----
library(quanteda) # main text package
library(readtext) # flexibly read in text
library(tidyverse) # for dplyr, stringr, piping, etc.
library(RColorBrewer) # better colors in graphs
library(scales) # better breaks and labels in graphs
# library(RJSONIO) # alternative to reading in JSON

setwd("~/Box Sync/mpc/textanalysis2018/") # set this to where you put the workshop materials

# 1. Acquire text ----
# a. comments scraped from Ours to Shape: http://ourstoshape.virginia.edu/
#    see acquire_comments.R
# b. class schedule data from Lou's list: https://rabi.phys.virginia.edu/mySIS/CS2/page.php?Semester=1188&Type=Group&Group=CS
# c. tweets from Trump Twitter Archive: http://trumptwitterarchive.com/archive
# d. novels from Project Gutenberg: http://www.gutenberg.org/


# 2. Read text into R ----
# readtext
# a. ours to shape comments
comments <- readtext("data/ots_comments.csv", text_field = "comment")
# Alternatively, use read_csv or read.csv:
# comments <- read_csv("ourstoshape/ots_comments.csv")

# b. trump tweets
tweets <- readtext("data/trumptweets/*.json", text_field = "text")
# Alternatively, use RJSONIO:
# files <- dir("data/trumptweets/", pattern=".json") # vector of all json file names
# tw_list <- map(paste0("data/trumptweets/", files), fromJSON, encoding = "UTF8") # read in JSON files
# tweets <-  map_df(tw_list, bind_rows) # convert list of dfs into one df

# c. class descriptions
# YOUR TURN: 
# try reading in the class schedule; it's in "data/louslist/CS1188Data.csv"
class <- readtext("data/louslist/CS1188Data.csv", text_field = "Description")

# d. book
sherlock_holmes <- texts(readtext("http://www.gutenberg.org/cache/epub/1661/pg1661.txt"))
str_sub(sherlock_holmes, 1, 90)

rm(class, sherlock_holmes)


# 3. Clean up text data ----
# a. ours to shape comments
# simplify doc_id
comments <- comments %>% mutate(doc_id = str_sub(doc_id, 18))
# multiple roles could be selected; i want binary indicators 
table(comments$role)
comments <- comments %>% 
  mutate(alum = ifelse(str_detect(role, "Alumna/us"), 1, 0),
         community = ifelse(str_detect(role, "Community Member"), 1, 0),
         faculty = ifelse(str_detect(role, "Faculty"), 1, 0),
         parent = ifelse(str_detect(role, "Parent"), 1, 0),
         staff = ifelse(str_detect(role, "Staff"), 1, 0),
         student = ifelse(str_detect(role, "Student"), 1, 0),
         supporter = ifelse(str_detect(role, "Supporter"), 1, 0))
# number of roles
comments$numroles <- rowSums(comments[,6:12])

# b. trump tweets
# create indicator for iphone and median split for favorite_count
table(tweets$source)
tweets <- tweets %>% mutate(iphone = ifelse(source == "Twitter for iPhone", "iphone", "not_iphone"),
                            iphone = as.factor(iphone),
                            favorite_hi = favorite_count > median(favorite_count))
tweets$date <- paste0(str_sub(tweets$created_at, 5, 10), ",", str_sub(tweets$created_at, 27,30))
tweets$date <- as.Date(tweets$date, "%b %d,%Y") # format as date
# simplify doc_id
tweets <- tweets %>% mutate(file_id = str_sub(doc_id, 1, 14),
                            doc_id = str_sub(doc_id, 21))


# 4. Create corpus, assign docvars ----
# a. ours to shape comments
comments_corpus <- corpus(comments) # generate corpus object

comments_corpus
summary(comments_corpus, 5) # list docvars
texts(comments_corpus)[1:5] # extract text

# document variable information can be extracted or assigned with docvars
docvars(comments_corpus, "type")

# b. trump tweets
# YOUR TURN: 
# create a corpus with the Trump tweets: tweets_corpus
# how many documents, how many document variables?
# what are the first five tweets read in?
tweets_corpus <- corpus(tweets)

# 5. Explore corpus and metadata ---- 
# a. ours to shape comments
# metadata
table(comments$type) # comment category
summary(comments) # commenter roles
# add number of tokens and sentence length to data frame
comments$words <- ntoken(comments_corpus) # add number of words/tokens
comments$sentences <- nsentence(comments_corpus) # add number of sentences
# if we read in a corpus with metada and wanted to create a dataframe of the docvars
# comments_meta <- summary(comments_corpus, n = nrow(comments)) # assigns all information in summary to dataframe
comments %>% group_by(type) %>% summarize(mean(words), sd(words))

# key words in context
kwic(comments_corpus, "equity")
kwic(comments_corpus, "service", window = 2) # change number of surrounding words
kwic(comments_corpus, "rac*", window = 3) # wildcard in pattern
kwic(comments_corpus, phrase("community service"), 2) # look for phrase
kwic(comments_corpus, "tradit", 3, valuetype = "regex") # pattern is regular expression

# readability
comment_read <- textstat_readability(comments_corpus, measure = "Flesch.Kincaid")
comments$read <- comment_read$Flesch.Kincaid # add to initial data frame
comments %>% group_by(numroles) %>% summarize(mean(read), sd(read), n())

# Which comments are the most complex/least readable?
comments %>% 
  filter(read > 25) %>% 
  select("type", "read", "text")

# b. trump tweets
# YOUR TURN: metadata
# how many tweets came from the iphone?
# add the number of tokens and sentences in each tweet to the tweet data frame
summary(tweets$iphone)
tweets$words <- ntoken(tweets_corpus) # add number of words/tokens
tweets$sentences <- nsentence(tweets_corpus) # add number of sentences

# which tweet has been favorited the most?
tweets %>% slice(which.max(favorite_count)) %>% select(date, text) 
# tweets %>% filter(favorite_count == max(favorite_count)) %>% select(text)

# YOUR TURN: kwic
# find trump's use of the win, winning, winner in the tweets
# pick any phrase, and find if/how it's used in the tweets
kwic(tweets_corpus, c("win", "winning", "winner"), 2)

# A graph, for fun: number of tweets by day
ggplot(tweets, aes(x=date, color=iphone)) + 
  geom_point(stat="count")
# better version
date.vec <- seq(from=as.Date("2017-01-20"), to=as.Date("2018-10-05"), by="weeks") # create date breaks to get chosen tick marks on graph
ggplot(tweets, aes(x=date, color=iphone)) + geom_point(stat="count") + 
  scale_x_date(labels = date_format("%m/%d"), breaks=date.vec) +   # label dates and coerce date tick marks
  scale_color_manual(values=c("orange3","blue3"), name="Source") + # set color for points and legend title
  labs(title = "Number of Trump Tweets",                           # graph title, subtitle, axis titles
       subtitle = "January 20, 2017 to September 30, 2018", 
       x="Date", y="Tweet Count") +
  theme(axis.text.x=element_text(angle=90, size=8),                # angle axis labels, set title size and bold
        plot.title = element_text(face="bold", size=18, hjust=0), 
        axis.title = element_text(face="bold", size=16),
        panel.grid.minor = element_blank(),
        legend.position="bottom")

# YOUR TURN: readability
# calculate the Flesch Kincaid readability score for the Trump tweets
# and add the value to the tweet data frame (call it "read")
tweets_read <- textstat_readability(tweets_corpus, measure = "Flesch.Kincaid")
tweets$read <- tweets_read$Flesch.Kincaid # add to initial data frame
tweets %>% group_by(iphone) %>% summarize(mean(read), sd(read))

# plot over time
tweets %>% mutate(favorite_hi = favorite_count > median(favorite_count)) %>% 
ggplot(aes(x = date, y = read)) + 
  geom_point(aes(color=favorite_hi), alpha=0.5, size=2) +
  geom_smooth(aes(color=favorite_hi)) +
  scale_x_date(labels = date_format("%m/%d"), breaks=date.vec) + 
  labs(title = "'Readability' of Trump Tweets",
       subtitle = "Flesch Kincaid Readability Index",
       y = "Grade level", x = "Date") +
  scale_color_manual(values=c("orange3","blue3"), name="Favorited (High)") +
  theme(plot.title = element_text(face="bold", size=18, hjust=0),
        axis.title = element_text(face="bold", size=16),
        panel.grid.minor = element_blank(), legend.position = c(0.9,0.9),
        axis.text.x = element_text(angle=90),
        legend.text=element_text(size=10))


# 6. Process and prepare text ----
# a. ours to shape comments
# tokenize (bag-of-words)
comments_tokens <- tokens(comments_corpus)
head(comments_tokens[[20]], 50) # first 50 tokens in 20th document

# remove punctuation
comments_tokens <- tokens(comments_corpus, remove_punct = TRUE)
head(comments_tokens[[20]], 50)
# additional arguments: remove_numbers, remove_hyphens, remove_separators [http://www.fileformat.info/info/unicode/category/index.htm]
#   remove_url [http(s)], remove_twitter [@, #]

# collocation analysis, finding multi-word expressions, ngrams/phrases
# first remove stopwords (but keep spaces to preserve non-adjacency)
stopwords("english") # what are stopwords
comments_tokens <- tokens_remove(comments_tokens, stopwords("en"), padding = TRUE)
head(comments_tokens[[20]], 50)
# generate collocations
comments_col <- textstat_collocations(comments_tokens, min_count = 10, tolower = FALSE)
head(comments_col, 50)

# the same process as above more compactly (using pipes)
comments_tokens <- tokens(comments_corpus, remove_punct = TRUE)
comments_col <- comments_tokens %>% 
  tokens_remove(stopwords("en"), padding = TRUE) %>% 
  textstat_collocations(min_count = 10, tolower = FALSE)
head(comments_col, 50)

# retain important multi-word expressions
comments_comptokens <- tokens_compound(comments_tokens, comments_col[c(2,9,24,30)])
head(comments_tokens[[50]],50) # before compounding
head(comments_comptokens[[50]],50) # after compounding
# kwic can be called on token objects as well
head(kwic(comments_comptokens, c('Thomas_Jefferson')))

# b. trump tweets
# YOUR TURN: 
# try tokenizing the trump tweets, remove punctuation, possibly numbers
# remove stopwords
# and find the high probability collocations
tweets_tokens <- tokens(tweets_corpus, remove_punct = TRUE, 
                        remove_numbers = TRUE,
                        remove_url = TRUE, verbose = TRUE)
tweets_tokens <- tokens_tolower(tweets_tokens)
stopwords2 <- stopwords()[c(1:173,175)]
tweets_tokens <- tokens_remove(tweets_tokens, c(stopwords2, "rt", "amp"), padding = TRUE)
head(tweets_tokens[[1]], 50)

tweets_col <- textstat_collocations(tweets_tokens, min_count = 10)
head(tweets_col, 50)
# retain important multi-word expressions
tweets_comptokens <- tokens_compound(tweets_tokens, tweets_col[c(1,4,6,7,8,16,17,30,32,39,41,42,45,47,49)])
kwic(tweets_comptokens, "hillary_clinton", 2)
head(tweets_comptokens[[300]],50) # after compounding


# 7. Describe text data from a dfm ----
# a. ours to shape comments
# dfm: create a document feature matrix from a corpus or a tokens object (if compounding used)
comments_dfm <- dfm(comments_corpus, remove_punct = TRUE, 
                    remove = stopwords(), verbose = TRUE) 
comments_dfm
# TO DO: add remove_punct = TRUE; remove = stopwords(); stem = TRUE

# list most frequent words
topfeatures(comments_dfm, 50) 

# wordcloud of most frequent words
textplot_wordcloud(comments_dfm, min_count = 25)
# fancier wordcloud
pal1 <- brewer.pal(9, "Blues") # define a color palette
set.seed(12)
textplot_wordcloud(comments_dfm, max_words = 200, color = pal1, rotation = 0.25) 

# plot of most frequent words
comments_freq <- textstat_frequency(comments_dfm, n = 100)
ggplot(comments_freq, aes(x = reorder(feature, frequency), y = frequency)) + 
  geom_point() + 
  labs(title = "Most Frequently Mentioned Words", 
       subtitle = "Ours to Shape Comments", 
       x = NULL, y = "Frequency") +
  theme_minimal(base_size = 10) + coord_flip()

# complexity/lexical diversity
lexdiv <- textstat_lexdiv(comments_dfm, "CTTR")
comments[,ncol(comments)+1] <- lexdiv[, 2]
comments %>% group_by(type) %>% summarize(mean(CTTR), sd(CTTR))

# b. trump tweets
# create a document-feature matrix from the tweet corpus
# consider removing punctuation, urls (remove_url = TRUE), and stopwords to start
# how many featuers are left, how sparse is the matrix?
tweets_dfm <- dfm(tweets_comptokens, remove = "", verbose = TRUE) 
tweets_dfm

# trim to reduce sparsity: dfm_trim, e.g., min_termfreq = 5, min_docfreq = 2
tweets_dfm_trim <- dfm_trim(tweets_dfm, min_termfreq = 5, min_docfreq = 2)
tweets_dfm_trim

# list most frequent words in trump's tweets
topfeatures(tweets_dfm_trim)

# create a wordcloud of the most frequent words in trump's tweets
pal1 <- brewer.pal(9, "Oranges")[2:9] # here's the color palette i'll use
set.seed(12)
textplot_wordcloud(tweets_dfm_trim, max_words = 200, color = pal1, rotation = 0.25) 

# plot the most frequent words
tweets_freq <- textstat_frequency(tweets_dfm_trim, n = 100)
ggplot(tweets_freq, aes(x = reorder(feature, frequency), y = frequency)) + 
  geom_point() + 
  labs(title = "Most Frequently Mentioned Words", 
       subtitle = "Trump Tweets", 
       x = NULL, y = "Frequency") +
  theme_minimal(base_size = 10) + coord_flip()


# 8. Compare text data ----
# a. ours to shape comments
# grouping, comparison cloud
dfm(comments_corpus, groups = "type", remove_punct = TRUE, 
    tolower = TRUE, remove = stopwords("english")) %>% 
  textplot_wordcloud(comparison = TRUE, max_words = 200,
                     color = c("orange3", "blue3", "turquoise3"),
                     min_size = .25, max_size = 4, labelsize = 1)

# relative frequency
# to compare across groups where the document length may not be equal,
# we need to weight the dfm (feature count divided by tokens)
comments_dfm_prop <- dfm_weight(comments_dfm, scheme = "prop")
# then calculate relative frequency by a document variable
comments_relfreq <- textstat_frequency(comments_dfm_prop, n = 50, groups = "type")
# and look at it
ggplot(comments_relfreq, aes(x = nrow(comments_relfreq):1, y = frequency)) +
  geom_point() + facet_wrap(~ group, scales = "free") +
  scale_x_continuous(breaks = nrow(comments_relfreq):1,
                     labels = comments_relfreq$feature) +
  labs(x = NULL, y = "Relative frequency") +
  coord_flip() 

# keyness: differential association of keywords in a target and reference group
# we're going to subset the dfm to contain only community and servie comments
# (to more clearly compare service to community, not service to everything else)
comments_dfm_nodisc <- dfm_subset(comments_dfm, 
                                  type %in% c("community", "service"))
# define the groups
comments_dfm_nodisc <- dfm_group(comments_dfm_nodisc, groups = "type")
# specify a target value from the groups
comments_key_nodisc <- textstat_keyness(comments_dfm_nodisc, 
                                        target = "service")
# plot
textplot_keyness(comments_key_nodisc)

# b. trump tweets
# grouping, comparison cloud
dfm(tweets_dfm_trim, groups = "favorite_hi") %>% 
  textplot_wordcloud(comparison = TRUE, max_words = 200,
                     color = c("orange3", "blue3", "turquoise3"),
                     min_size = .25, max_size = 3, labelsize = 1)

# relative frequency
tweets_dfm_prop <- dfm_weight(tweets_dfm_trim, scheme = "prop")
# then calculate relative frequency by a document variable
tweets_relfreq <- textstat_frequency(tweets_dfm_prop, n = 50, groups = "favorite_hi")
# and look at it
ggplot(tweets_relfreq, aes(x = nrow(tweets_relfreq):1, y = frequency)) +
  geom_point() + facet_wrap(~ group, scales = "free") +
  scale_x_continuous(breaks = nrow(tweets_relfreq):1,
                     labels = tweets_relfreq$feature) +
  labs(x = NULL, y = "Relative frequency") +
  coord_flip() 

# keyness
tweets_key <- tweets_dfm_trim %>% 
  dfm_group(groups = "favorite_hi") %>% 
  textstat_keyness(target = "TRUE")
textplot_keyness(tweets_key, n = 25)


# clean up and save for later ----
rm(comment_read, comments_col, comments_dfm_nodisc, 
   comments_dfm_prop, comments_freq, comments_key_nodisc, 
   comments_relfreq, lexdiv, tweets_col, tweets_dfm_prop, 
   tweets_freq, tweets_key, tweets_read, tweets_relfreq, 
   date.vec, pal1, stopwords2)
save.image("text1.RData")
