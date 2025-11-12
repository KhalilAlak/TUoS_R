install.packages('tm')
install.packages('SnowballC')
install.packages('glmnet')
library(tm)
library(glmnet)
library(SnowballC)
library(tidyverse)

topic_docs <- Corpus(
  DirSource(
    "20news-train/comp.graphics",
    encoding='UTF-8'
  )
)

summary(topic_docs[1:5])

inspect(topic_docs[[1]])

topic_docs[[1]]$meta
topic_docs[[1]]$content

topic_2_docs <- Corpus(
  DirSource(
    "20news-train/rec.motorcycles",
    encoding='UTF-8'
  )
)

binomial_docs <- c(
  as.list(topic_docs),
  as.list(topic_2_docs)
)

labels_1 <- replicate(length(topic_docs), 'comp.graphics')
labels_2 <- replicate(length(topic_2_docs), 'rec.motorcycles')
binomial_labels <- c(labels_1, labels_2)

length(binomial_docs) == length(binomial_labels)

example_doc <- binomial_docs[["38487"]]
print(example_doc)

tokens <- Boost_tokenizer(example_doc)
summary(tokens)

#Execise 1
tokens <- MC_tokenizer(example_doc)
summary(tokens)

view(tokens)
#End of Execise

removePunctuation(example_doc)

removePunctuation(tokens)

stops <- stopwords('en')
print(stops)

removeWords(example_doc, stops)

tolower(example_doc)

removeWords(
  tolower(example_doc),
  stops
)

#Execise 2
tokens <- strsplit(example_doc, "\\s+")[[1]]
print(tokens[1:20]) 

tokens_lower <- tolower(tokens)

library(tm)
stops <- stopwords("en")
tokens_clean <- removeWords(tokens_lower, stops)
print(tokens_clean[1:20])
#End of Execise

removeWords(
  tolower(tokens),
  stops
)

stemDocument(
  example_doc
)
#Execise 3

text_lower <- tolower(example_doc)

text_nopunct <- removePunctuation(text_lower)

stops <- stopwords("en")
text_nostop <- removeWords(text_nopunct, stops)

text_stemmed <- stemDocument(text_nostop)

print(text_stemmed)

#End of Execise

binomial_docs <- Corpus(
  VectorSource(binomial_docs)
)
binomial_docs

# Start with lowercasing
cleaned_binomial_docs <- tm_map(
  binomial_docs, # the collection of documents to process
  tolower # the function to apply to each document
)
cleaned_binomial_docs$content[1] # view the document - are they in lowercase now?

cleaned_binomial_docs$content[1]
# Then remove punctuation
cleaned_binomial_docs <- tm_map(
  cleaned_binomial_docs, # we want to stack on top of the previous preprocessing!
    removePunctuation # the function to apply to each document
)
cleaned_binomial_docs$content[1] # have all punctuations been removed?
# Then remove stopwords
cleaned_binomial_docs <- tm_map(
  cleaned_binomial_docs,
  removeWords, # the function to apply to each document
  stopwords('en') # additional argument to specify the stopwords to be removed
)
cleaned_binomial_docs$content[1] # have the stopwords been removed?
# And finally stem
cleaned_binomial_docs <- tm_map(
  cleaned_binomial_docs,
  stemDocument
)
cleaned_binomial_docs$content[1] # have the words been stemmed?

binomial_dtm <- DocumentTermMatrix(
  cleaned_binomial_docs
)
binomial_dtm

#Execise 4
binomial_dtm <- DocumentTermMatrix(cleaned_binomial_docs)

original_dtm <- DocumentTermMatrix(binomial_docs)
original_dtm

#End of Execise

inspect(binomial_dtm[1:3,])

doc_lengths <- lapply( # applies a function across each element of a list
  as.list(cleaned_binomial_docs),
  nchar # count the length of a string
)
doc_lengths <- unlist(doc_lengths) # get rid of the structure that lapply() creates
quantile(doc_lengths)

binomial_dtm_binary <- DocumentTermMatrix(
  cleaned_binomial_docs,
  control=list(
    weighting=weightBin
  )
)

inspect(binomial_dtm_binary[1:3,])

binomial_dtm_tfidf <- DocumentTermMatrix(
  cleaned_binomial_docs,
  control=list(
    weighting=weightTfIdf
  )
)

inspect(binomial_dtm_tfidf[1:3,])

removeSparseTerms(binomial_dtm, 0.98)

#Execise 5
library(tm)

corpus <- cleaned_binomial_docs

dtm <- DocumentTermMatrix(corpus)

dtm

dtm_sparse <- removeSparseTerms(dtm, 0.98)
dtm_sparse

removeSparseTerms(dtm, sparse = 0.95)

binomial_dtm

ncol(removeSparseTerms(binomial_dtm, 0.99))   # threshold = 0.99
ncol(removeSparseTerms(binomial_dtm, 0.98))   # threshold = 0.98
ncol(removeSparseTerms(binomial_dtm, 0.95))   # threshold = 0.95
ncol(removeSparseTerms(binomial_dtm, 0.90))   # threshold = 0.90
ncol(removeSparseTerms(binomial_dtm, 0.80))   # threshold = 0.80

#End of Execise

binomial_dtm
binomial_train_dtm
dim(binomial_train_dtm)

observed_vocabulary <- Terms(binomial_dtm)

observed_vocabulary <- binomial_dtm$dimnames$Terms

length(observed_vocabulary)

binomial_train_dtm <- DocumentTermMatrix(
  cleaned_binomial_docs,
  control = list(dictionary = observed_vocabulary)
)
dim(binomial_train_dtm)

library(glmnet)

binomial_model <- glmnet(
  as.matrix(binomial_train_dtm),
  binomial_train_labels,
  family = 'binomial'
)

