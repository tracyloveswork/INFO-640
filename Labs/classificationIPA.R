# Conversational Agents Terms
# Text Analysis | Topic Modeling 

#to create and work with corpora
install.packages("tm")
YYyes#for LDA topic models
install.packages("topicmodels")
install.packages('qdap')
install.packages('textometry')
install.packages("quanteda")


library(tidytext)
library(tm)
library(stringr)
library(topicmodels)
library(tidyverse)
library(dplyr)
library(data.table)
library(textometry)
library(quanteda)

# Loading combined datasets for Medium and Google Scholar
# load your dataset and set "stringsAsFactors" to FALSE to override the fact that R will read text as a factor rather than a character
scholar_alexa <- read.csv("../DataSets/IPA_datasets/combined_gs.csv", encoding = "utf-8", header=TRUE, stringsAsFactors = FALSE)
medium_alexa <- read.csv("../DataSets/IPA_datasets/combined_m.csv", encoding = "utf-8", header=TRUE, stringsAsFactors = FALSE)

# VUIs
# load your dataset and set "stringsAsFactors" to FALSE to override the fact that R will read text as a factor rather than a character
vui_scholar_alexa <- read.csv("../DataSets/IPA_datasets/vui_gs.csv", encoding = "utf-8", header=TRUE, stringsAsFactors = FALSE)
vui_medium_alexa <- read.csv("../DataSets/IPA_datasets/vui_m.csv", encoding = "utf-8", header=TRUE, stringsAsFactors = FALSE)

# Create Corpus
# Specify the data frame source
scholar_source <- DataframeSource(scholar_alexa)
medium_source <- DataframeSource(medium_alexa)

vui_scholar_source <- DataframeSource(vui_scholar_alexa)
vui_medium_source <- DataframeSource(vui_medium_alexa)

# Transform into a corpus
scholar_corpus <- VCorpus(scholar_source)
medium_corpus <- VCorpus(medium_source)

vui_scholar_corpus <- VCorpus(vui_scholar_source)
vui_medium_corpus <- VCorpus(vui_medium_source)

# Now our text column has been transformed into a special type of format: a corpus. 
print(vui_scholar_corpus)
# Using indexing we can call on a specific record. This gives metadat on that record.
print(scholar_corpus[[10]])
print(scholar_corpus[[10]][1])

# Clean the Corpus - scholar
scholar_cleaned <- tm_map(scholar_corpus, removeNumbers)
scholar_cleaned <- tm_map(scholar_cleaned, stripWhitespace)
scholar_cleaned <- tm_map(scholar_cleaned, removePunctuation)
scholar_cleaned <- tm_map(scholar_cleaned, content_transformer(tolower))

# Clean the Corpus - medium
medium_cleaned <- tm_map(medium_corpus, removeNumbers)
medium_cleaned <- tm_map(medium_cleaned, stripWhitespace)
medium_cleaned <- tm_map(medium_cleaned, removePunctuation)
medium_cleaned <- tm_map(medium_cleaned, content_transformer(tolower))

# Clean the Corpus - vui_scholar
vui_scholar_cleaned <- tm_map(vui_scholar_corpus, removeNumbers)
vui_scholar_cleaned <- tm_map(vui_scholar_cleaned, stripWhitespace)
vui_scholar_cleaned <- tm_map(vui_scholar_cleaned, removePunctuation)
vui_scholar_cleaned <- tm_map(vui_scholar_cleaned, content_transformer(tolower))

# Clean the Corpus - vui_medium
vui_medium_cleaned <- tm_map(vui_medium_corpus, removeNumbers)
vui_medium_cleaned <- tm_map(vui_medium_cleaned, stripWhitespace)
vui_medium_cleaned <- tm_map(vui_medium_cleaned, removePunctuation)
vui_medium_cleaned <- tm_map(vui_medium_cleaned, content_transformer(tolower))

#stopwords - Everbody gets the same treatment
scholar_stopwords <- c(stopwords("en"),"can", "interaction", "interact", "vui", "vuis","designed")
scholar_cleaned <- tm_map(scholar_cleaned, removeWords, scholar_stopwords)
vui_scholar_cleaned <- tm_map(vui_scholar_cleaned, removeWords, scholar_stopwords)
scholar_cleaned[[2]][1]
medium_stopwords <- c(stopwords("en"),"can", "user", "used", "users")
medium_cleaned <- tm_map(medium_cleaned, removeWords, medium_stopwords)
vui_medium_stopwords <- c(stopwords("en"),"can", "design","user", "used","“", "”", "alexa","fred", "rogers", "interface", "the","users", "voice", "vui", "voices", "the", "like", "voic")
vui_medium_cleaned <- tm_map(vui_medium_cleaned, removeWords, vui_medium_stopwords)
vui_medium_cleaned[[2]][1]

# Stemming
scholar_cleaned <- tm_map(scholar_cleaned, stemDocument, "english")
medium_cleaned <- tm_map(medium_cleaned, stemDocument, "english")
vui_scholar_cleaned <- tm_map(vui_scholar_cleaned, stemDocument, "english")
vui_medium_cleaned <- tm_map(vui_medium_cleaned, stemDocument, "english")
vui_medium_cleaned <- tm_map(vui_medium_cleaned, removeWords, vui_medium_stopwords)

head(vui_medium_cleaned)

# Topic Modeling
# Begin by creating a document-term matrix from our clean dataset
scholar_dtm <- DocumentTermMatrix(scholar_cleaned)
medium_dtm <- DocumentTermMatrix(medium_cleaned)
vui_scholar_dtm <- DocumentTermMatrix(vui_scholar_cleaned)
vui_medium_dtm <- DocumentTermMatrix(vui_medium_cleaned)

# Get rid of empty rows, if there were any :P
unique_indexes_scholar <- unique(scholar_dtm$i)
scholar_dtm <- scholar_dtm[unique_indexes_scholar,]
scholar_dtm

unique_indexes_medium <- unique(medium_dtm$i)
medium_dtm <- medium_dtm[unique_indexes_medium,]
medium_dtm

unique_indexes_vui_scholar <- unique(vui_scholar_dtm$i)
vui_scholar_dtm <- vui_scholar_dtm[unique_indexes_vui_scholar,]
vui_scholar_dtm

unique_indexes_vui_medium <- unique(vui_medium_dtm$i)
vui_medium_dtm <- medium_dtm[unique_indexes_vui_medium,]
vui_medium_dtm

# Get it ready for Tidy! - scholar
scholar_dtm_tidy <- tidy(scholar_dtm)
scholar_dtm_tidy

cleaned_scholar_res <- scholar_dtm_tidy %>%
  group_by(document) %>%
  mutate(terms = toString(rep(term, count))) %>%
  select(document, terms) %>%
  unique()

head(cleaned_scholar_res)

# Get it ready for Tidy! - medium
medium_dtm_tidy <- tidy(medium_dtm)
medium_dtm_tidy

cleaned_medium_res <- medium_dtm_tidy %>%
  group_by(document) %>%
  mutate(terms = toString(rep(term, count))) %>%
  select(document, terms) %>%
  unique()

head(cleaned_medium_res)

# Get it ready for Tidy! - vui_scholar
vui_scholar_dtm_tidy <- tidy(vui_scholar_dtm)
vui_scholar_dtm_tidy

cleaned_vui_scholar_res <- vui_scholar_dtm_tidy %>%
  group_by(document) %>%
  mutate(terms = toString(rep(term, count))) %>%
  select(document, terms) %>%
  unique()

head(cleaned_vui_scholar_res)

# Get it ready for Tidy! - vui_medium
vui_medium_dtm_tidy <- tidy(vui_medium_dtm)
vui_medium_dtm_tidy

cleaned_vui_medium_res <- vui_medium_dtm_tidy %>%
  group_by(document) %>%
  mutate(terms = toString(rep(term, count))) %>%
  select(document, terms) %>%
  unique()

head(cleaned_vui_medium_res)


# Run LDA from Topic Modeling Package. We want 6 topics, you can choose more or less. 
# We’ll put 5 terms into each topic.
k <- 5

# scholar
scholar_lda <- LDA(scholar_dtm, k = k, control = list(seed=1234))
scholar_lda # A LDA_VEM topic model with 6 topics.
scholar_lda_words <- terms(scholar_lda,5)
# That gives us the top 5 terms for each of 6 topics.

# medium
medium_lda <- LDA(medium_dtm, k = k, control = list(seed=1234))
medium_lda # A LDA_VEM topic model with 6 topics.
medium_lda_words <- terms(medium_lda,5)
# That gives us the top 5 terms for each of 6 topics.

# vui_scholar
vui_scholar_lda <- LDA(vui_scholar_dtm, k = k, control = list(seed=1234))
vui_scholar_lda # A LDA_VEM topic model with 6 topics.
vui_scholar_lda_words <- terms(vui_scholar_lda,5)
# That gives us the top 5 terms for each of 6 topics.

# vui_medium
vui_medium_lda <- LDA(vui_medium_dtm, k = k, control = list(seed=1234))
vui_medium_lda # A LDA_VEM topic model with 6 topics.
vui_medium_lda_words <- terms(vui_medium_lda,5)
# That gives us the top 5 terms for each of 6 topics.


scholar_lda_topics <-as.matrix(scholar_lda_words)
head(scholar_lda_topics)
write.csv(scholar_lda_topics,file=paste("scholar_LDA_",k,".csv"))

# Use indeed_lda_words to create the *_lda_topics matrix and save as a csv.
medium_lda_topics <-as.matrix(medium_lda_words)
head(medium_lda_topics)
write.csv(medium_lda_topics,file=paste("medium_LDA_",k,".csv"))

# ---
# To visualize as a table, we will need to tidy and create a bar chart.
scholar_lda_tidy <- tidy(scholar_lda)
scholar_lda_tidy
# We now have a statistic for how much each word is associated with a topic.

# Order words from most prominent to least for each topic
top_terms <- scholar_lda_tidy %>%
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
# ---

# Function that does all above with any corpus
get_LDA_topics_terms_by_topic <- function(input_corpus, plot = TRUE, number_of_topics = 6, number_of_words = 5) {
  my_dtm <- DocumentTermMatrix(input_corpus)
  unique_indexes <- unique(my_dtm$i)
  my_dtm <- my_dtm[unique_indexes,]
  my_lda <- LDA(my_dtm, k = number_of_topics, control = list(seed=1234))
  my_topics <- tidy(my_lda, matrix="beta")
  my_lda_words <- terms(my_lda, number_of_words)
  my_lda_topics <- as.matrix(my_lda_words)
  write.csv(my_lda_topics,file=paste("indeed_LDA_function",k,".csv"))
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
get_LDA_topics_terms_by_topic(scholar_cleaned)

# With the function we can change parameters
get_LDA_topics_terms_by_topic(scholar_cleaned, number_of_topics = 4, number_of_words = 5)
get_LDA_topics_terms_by_topic(medium_cleaned, number_of_topics = 5, number_of_words = 5)
get_LDA_topics_terms_by_topic(vui_scholar_cleaned, number_of_topics = 3, number_of_words = 3)
get_LDA_topics_terms_by_topic(vui_medium_cleaned, number_of_topics = 3, number_of_words = 3)


#--
# Rather than seeing top topics across the corpus, we would like it arranged by document
# Gamma calculation with LDA Vector

scholar_lda_document_topics <- tidy(scholar_lda, matrix="gamma")
scholar_lda_document_topics
scholar_lda_document_topics$document <- as.integer(scholar_lda_document_topics$document)

write.csv(scholar_lda_document_topics,file=paste("scholar_LDA_document_topics_",k,".csv"))
head(scholar_lda_document_topics)

# Need additional variables to analyze
dt1 <- data.table(scholar_lda_document_topics, key = "document")
dt2 <- data.table(scholar_alexa, key = "doc_id")

scholar_merged <- dt1[dt2]
dim(scholar_merged)
colnames(scholar_merged)

scholar_analyze <- select(scholar_merged, c(title))
head(scholar_analyze)

