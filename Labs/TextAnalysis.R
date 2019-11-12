# Text Analysis
# Cleaning corpora based on the type of analysis you want to do | Supervised Sentiment Analysis | Topic Modeling with LDA

#to import the books from project gutenberg directly
install.packages("gutenbergr")
#to create and work with corpora
install.packages("tm")
YYyes#for LDA topic models
install.packages("topicmodels")


library(tidytext)
library(tm)
library(stringr)
library(topicmodels)
library(gutenbergr)
library(tidyverse)
library(dplyr)
library(data.table)

# Direct Text

# Emily Dickenson Poem

text <- "Because I could not stop for Death -
He kindly stopped for me -
The Carriage held but just Ourselves -
and Immortality"

text

# Focusing on word use, we’ll make everything lowercase, remove any punctuation, and remove extraneous whitespace.

# make lowercase
tolower(text)

# remove punctuation
removePunctuation(text)

# remove whitespace
stripWhitespace(text)
# nothing as been saved since we have not reassigned the variable
text

# more advanced cleaning can be done with the qdap package  
install.packages('qdap')

# Stopwords are words that are exceedingly common, such as determiners, prepositions, pronouns, 
# auxiliaries, etc. “the, a, of, in, he, she, is, have”. The tm package has some built in stopwords.

# inspect different stopwords lists with the 2 letter abbreviation for a language.
stopwords("en")
# remove stopwords from the text object. Notice that capitalized words are not removed.
removeWords(text, stopwords("en"))

# Sometimes you will need to add words to your stopwords list.
# you’d need to remove the target because it will be too prominent in your corpus.
my_stops <- c(stopwords("en"), "death")

# 1. Transform text to lowercase and save into a new variable (call it text_new)
text_new <- tolower(text)

# 2. Strip the whitespace and remove the punctuation.
text_new <- removePunctuation(text_new) 
text_new <- stripWhitespace(text_new)

# 3. Add “death” to the stopwords list
my_stops <- c(stopwords("en"), "death")

# 4. Remove the stopwords from text_new
text_new <- removeWords(text_new, my_stops)

# 5. Print text_new to the screen.
text_new

# Stemming - When we are interested in meaning (rather than tense, genre, or inflection), 
# we want to treat these (i.e. “stop” and “stopped”) as the same word. This is called stemming.

# Using the stemCompletion function, and pass it our stemmed text and the original list of separated words.
# This ensures that every word in our stemmed corpus has a correlate in the lookup dictionary. 
# As you will see, the completion function maps back to the same, most basic word form.

# spilt the new_text on the spaces, then turn it from a list to a vector and assign it to a new variable.
nvec <- unlist(strsplit(text_new, split=" "))

# Then stem the items in this new variable using stemDocument().
stem_text <- stemDocument(nvec)

# Print it to the screen to inspect what happened.
stem_text
nvec
print(stem_text)

completed_text <- stemCompletion(stem_text,nvec)
completed_text

# Structured Text

# load your dataset and set "stringsAsFactors" to FALSE to override the fact that R will read text as a factor rather than a character

peace_res <- read.csv("../DataSets/pax_20_02_2018_1_CSV.csv",encoding = "utf-8")
# Check it out
str(peace_res)
glimpse(peace_res)
# This is a rectangular (flat) dataset, we are going to reshape it into a corpus (dictionary) form.

# We need to preserve the document ID so we can join the work we do on the corpus 
# and topic modeling to the other data in the dataframe.

# To make this work, we use the DataframeSource function and the text variable must be called “text” 
# and the document ID variable must be called “doc_id”.

# First, change the names.
names(peace_res)[names(peace_res)=="AgtId"] <- "doc_id"
# Covert to character
peace_res$doc_id <- as.character(peace_res$doc_id)
names(peace_res)[names(peace_res)=="OthAgr"] <- "text"
colnames(peace_res)
# Specify the data frame source
peace_source <- DataframeSource(peace_res)
glimpse(peace_source)
# Transform into a corpus
peace_corpus <- VCorpus(peace_source)

# Now our text column has been transformed into a special type of format: a corpus. 
# We can use functions in the tm package to analyze it that we could not do when it was a rectangular dataset.

# Metadata on the corpus
print(peace_corpus)
# Using indexing we can call on a specific record. This gives metadat on that record.
print(peace_corpus[[10]])
# Specify a particular entry by calling it's location within a record.
print(peace_corpus[[10]][1])
# Thefirst index is with 2 sets of brackets because you are calling the whole record.
# The second index is with one set of brackets because it is a level beneath the record: the entry.
print(peace_corpus[[10]][2])

# Make a new corpus to clean
# We cannot interact with the text directly like the first example - we must map the function onto the corpus.
peace_cleaned <- tm_map(peace_corpus, removeNumbers)
peace_cleaned[[10]][1]

# Use the tm_map function to finish cleaning the peace corpus:
# 1. Remove punctuation
peace_cleaned <- tm_map(peace_cleaned, removePunctuation)
# 2. Strip excess whitespace
peace_cleaned <- tm_map(peace_cleaned, stripWhitespace)
# 3. Make everything lowercase
# peace_cleaned <- tm_map(peace_cleaned, tolower)
peace_cleaned <- tm_map(peace_cleaned, content_transformer(tolower))
# 4. Add “peace” and “agreement” to your stopwords list (you do not need tm_map() for this)
my_stopwords <- c(stopwords("en"), "peace", "agreement", "page", "parties", "shall", "will")
# 5. Remove stopwords
peace_cleaned <- tm_map(peace_cleaned, removeWords, my_stopwords)
# 6. Inspect the output by looking at the text of the 5th entry.
peace_cleaned[[5]][1]
# 5th entry was empty :(
peace_cleaned[[4]][1]


# Topic Modeling
# A way to describe what topics are associated with a document and what words are associated with which topics.

# With Topic Modeling you can (1) cluster texts together, (2) get a general sense of a document or documents 
# and (3) identify dentifying themes across documents

# Begin by creating a document-term matrix from our clean dataset
peace_dtm <- DocumentTermMatrix(peace_cleaned)
# Gives error if you did not use content transformer when making everything lowercase

peace_dtm

# Get rid of empty rows
# Create a selection of unique indexes (have values)
unique_indexes <- unique(peace_dtm$i)

peace_dtm <- peace_dtm[unique_indexes,]
peace_dtm

peace_dtm_tidy <- tidy(peace_dtm)
peace_dtm_tidy

#possibly remove this section
cleaned_peace_res <- peace_dtm_tidy %>%
  group_by(document) %>%
  mutate(terms = toString(rep(term, count))) %>%
  select(document, terms) %>%
  unique()

head(cleaned_peace_res)

# Run LDA from Topic Modeling Package. We want 6 topics, you can choose more or less. 
# We’ll put 5 terms into each topic.
k <- 6
peace_lda <- LDA(peace_dtm, k = k, control = list(seed=1234))
peace_lda
peace_lda_words <- terms(peace_lda,5)
# That gives us the top 5 terms for each of 6 topics.

# Use peace_lda_words to create the peace_lda_topics matrix and save as a csv. 
# We use the paste fucntion to add the value of k to the filename.
peace_lda_topics <-as.matrix(peace_lda_words)
head(peace_lda_topics)
write.csv(peace_lda_topics,file=paste("../DataSets/peace_LDA_",k,".csv"))


# To visualize as a table, we will need to tidy and create a bar chart.
peace_lda_tidy <- tidy(peace_lda)
peace_lda_tidy

# We now have a statistic for how much each word is associated with a topic.

# Order words from most prominent to least for each topic
top_terms <- peace_lda_tidy %>%
  group_by(topic) %>%
  top_n(5, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)
top_terms

top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill=factor(topic))) +
  geom_col(show.legend=FALSE) +
  facet_wrap(~ topic, scales = "free")+
  coord_flip()
# NOTE: The best way to improve your topic models is to refine your stopwords heavily. 
# Removing stopwords will remove noise from the topics and make them more interpretable.

# EXERCISE: Are there any more stopwords you would want to add? Yes! "page" and maybe "parties", "shall", "will"
# my_stopwords <- c(stopwords("en"), "peace", "agreement", "page", "parties", "shall", "will")

# A More Portable Technique
# Write a function
get_LDA_topics_terms_by_topic <- function(input_corpus, plot = TRUE, number_of_topics = 6, number_of_words = 5) {
  my_dtm <- DocumentTermMatrix(input_corpus)
  unique_indexes <- unique(my_dtm$i)
  my_dtm <- my_dtm[unique_indexes,]
  my_lda <- LDA(my_dtm, k = number_of_topics, control = list(seed=1234))
  my_topics <- tidy(my_lda, matrix="beta")
  my_lda_words <- terms(my_lda, number_of_words)
  my_lda_topics <- as.matrix(my_lda_words)
  write.csv(my_lda_topics,file=paste("../DataSets/peace_LDA_function",k,".csv"))
  my_top_terms <- my_topics %>%
    group_by(topic) %>%
    top_n(number_of_words, beta) %>%
    ungroup() %>%
    arrange(topic, -beta)
  if(plot==TRUE){
    my_top_terms %>%
      mutate(term = reorder(term, beta)) %>%
      ggplot(aes(term, beta, fill=factor(topic))) +
      geom_col(show.legend=FALSE) +
      facet_wrap(~ topic, scales = "free")+
      coord_flip()
  }else{
    return(my_top_terms)
  }
}
get_LDA_topics_terms_by_topic(peace_cleaned)

# With the function we can change parameters
get_LDA_topics_terms_by_topic(peace_cleaned, number_of_topics = 4, number_of_words = 4)


# Rather than seeing top topics across the corpus, we would like it arranged by document
# Gamma calculation with LDA Vector

peace_lda_document_topics <- tidy(peace_lda, matrix="gamma")
peace_lda_document_topics

write.csv(peace_lda_document_topics,file=paste("../DataSets/peace_LDA_document_topics_",k,".csv"))
head(peace_lda_document_topics)


dt1 <- data.table(peace_lda_document_topics, key = "document")
dt2 <- data.table(peace_res, key = "doc_id")
peace_merged <- dt1[dt2]
dim(peace_merged)
colnames(peace_merged)

peace_analyze <- select(peace_merged, c(Con, Contp, Reg, Dat, Status, Lgt, Agtp))
head(peace_analyze)


# Unstructured Text
# Load Don Quixote
dq <- gutenberg_download(996)
dq

# Create a corpus, no need to worry about metadata
dq_source <- VectorSource(dq)
dq_corpus <- VCorpus(dq_source)
dq_source[[2]][1]
# This is the body
dq_corpus[[2]][1]

# 1. Clean and prepare the text of Don Quixote for topic modeling.
# Edit stop words
dq_stops <- c(stopwords("en"), "don", "quixote", "said", "though", "may", "will", "said", "sancho", "one", "thou", "thee")

dq_cleaned <- tm_map(dq_corpus, removeNumbers)
glimpse(dq_cleaned)
dq_cleaned <- tm_map(dq_cleaned, removePunctuation)
dq_cleaned <- tm_map(dq_cleaned, stripWhitespace)
dq_cleaned <- tm_map(dq_cleaned, content_transformer(tolower))
dq_cleaned <- tm_map(dq_cleaned, removeWords, dq_stops)

# Didn't need to do this :P
dq_dtm <- DocumentTermMatrix(dq_cleaned)
dq_dtm
# Get rid of empty rows
# Create a selection of unique indexes (have values)
unique_indexes <- unique(dq_dtm$i)
dq_dtm <- dq_dtm[unique_indexes,]
dq_dtm
dq_dtm_tidy <- tidy(dq_dtm)
dq_dtm_tidy

# 2. Call your topic modeling function to view the output.
get_LDA_topics_terms_by_topic(dq_cleaned)
# Add stopwords: don quixote said though may will said
# 3. Find 4, 5, 6, and 7 topics (any number of words, but keep it consistent).
get_LDA_topics_terms_by_topic(dq_cleaned, number_of_topics = 4, number_of_words = 4)
get_LDA_topics_terms_by_topic(dq_cleaned, number_of_topics = 5, number_of_words = 4)
get_LDA_topics_terms_by_topic(dq_cleaned, number_of_topics = 6, number_of_words = 4)
get_LDA_topics_terms_by_topic(dq_cleaned, number_of_topics = 7, number_of_words = 4)
# Be sure to change the name of the file that is outputted.

# 4. What do you notice? Did you have to revisit any of the terms? 
#Names and uncommon stop words such as thou and thee had to be removed

# Sentiment Analysis
install.packages("textdata")
library(textdata)
sentiments
data("sentiments")
glimpse(sentiments)
get_sentiments("afinn")
get_sentiments("nrc")
get_sentiments("bing")

lyrics_raw <- read.csv("../DataSets/songdata.csv",encoding = "utf-8", header=TRUE, check.names = TRUE, stringsAsFactors = FALSE)
summary(lyrics_raw)
glimpse(lyrics_raw)

tidy_lyrics <- lyrics_raw %>%
  ungroup() %>%
  unnest_tokens(word, text)

summary(tidy_lyrics)
head(tidy_lyrics)

nrc_sent <- get_sentiments("nrc") %>%
  filter(sentiment == "joy")
nrc_sent

# David Bowie
tidy_lyrics_bowie <- tidy_lyrics %>%
  filter(artist == "David Bowie")
tidy_lyrics_bowie

tidy_lyrics_bowie %>%
  inner_join(nrc_sent) %>%
  dplyr::count(word, sort = TRUE)

bowie_sentiment <- tidy_lyrics_bowie %>%
  inner_join(get_sentiments("bing")) %>%
  dplyr::count(song, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)
head(bowie_sentiment)

ggplot(bowie_sentiment, aes(negative, positive, color = song)) +
  geom_jitter(show.legend = FALSE)

bowie_career_sentiment <- mean(bowie_sentiment$sentiment)
bowie_career_sentiment
# -0.1042945 is slightly negative

unique(lyrics_raw$artist)

# Selena Gomez
tidy_lyrics_gomez <- tidy_lyrics %>%
  filter(artist == "Selena Gomez")
tidy_lyrics_gomez

tidy_lyrics_gomez %>%
  inner_join(nrc_sent) %>%
  dplyr::count(word, sort = TRUE)

gomez_sentiment <- tidy_lyrics_gomez %>%
  inner_join(get_sentiments("bing")) %>%
  dplyr::count(song, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)
head(gomez_sentiment)

ggplot(gomez_sentiment, aes(negative, positive, color = song)) +
  geom_jitter(show.legend = FALSE)

gomez_career_sentiment <- mean(gomez_sentiment$sentiment)
gomez_career_sentiment
# 5.77 is very positive

# Zac Brown Band
tidy_lyrics_brown <- tidy_lyrics %>%
  filter(artist == "Zac Brown Band")
tidy_lyrics_brown

tidy_lyrics_brown %>%
  inner_join(nrc_sent) %>%
  dplyr::count(word, sort = TRUE)

brown_sentiment <- tidy_lyrics_brown %>%
  inner_join(get_sentiments("bing")) %>%
  dplyr::count(song, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)
head(brown_sentiment)

ggplot(brown_sentiment, aes(negative, positive, color = song)) +
  geom_jitter(show.legend = FALSE)

brown_career_sentiment <- mean(brown_sentiment$sentiment)
brown_career_sentiment
# 3.516129 is positive