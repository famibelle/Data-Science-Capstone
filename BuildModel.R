library(splitstackshape)
library(e1071)
library(tm)
library(RWeka)
library(reshape2)
library(stringr)
library(shiny)
library(slam)
library(plyr)

# Loading the data
con <- file("../Data-Science-Capstone-Data/en_US.twitter.txt", "r")
twitter <- readLines(con)
close(con)

con <- file("../Data-Science-Capstone-Data/en_US.blogs.txt", "r")
# blogs <- readLines(con, encoding="UTF-8")
blogs <- readLines(con)
close(con)

con <- file("../Data-Science-Capstone-Data/en_US.news.txt", "r")
# news <- readLines(con, encoding="UTF-8")
news <- readLines(con)
close(con)
rm(con)

# Clean the data
## functions to create the Corpus

### Remove the non alphanumeric characters
removeNonAlphaCharacter <- function(string){
    gsub("[^[:alnum:] ]", "",string)
}

Create_Corpus <- function(sample) {
    Corpus <- VCorpus(VectorSource(sample), readerControl = list(language = "en"))
    Corpus <- tm_map(Corpus, removePunctuation)
    Corpus <- tm_map(Corpus, content_transformer(tolower))
#     Corpus <- tm_map(Corpus, removeNonAlphaCharacter)
    Corpus <- tm_map(Corpus, stripWhitespace)
    Corpus <- tm_map(Corpus, removeNumbers)
#   Corpus <- tm_map(Corpus, removeWords, Profanities)
#   Corpus <- tm_map(Corpus, stemDocument) # Stem document
    return(Corpus)
}

# Sample the data from the dataset
twitter <-  sample(twitter, size = 10000)
blogs <-    sample(blogs,   size = 10000)
news <-     sample(news,    size = 10000)

twitter.corpus  <- Create_Corpus(twitter); rm(twitter)
blogs.corpus    <- Create_Corpus(blogs); rm(blogs)
news.corpus     <- Create_Corpus(news); rm(news)

ALL <- c(twitter.corpus, news.corpus, blogs.corpus)
rm(twitter.corpus, news.corpus, blogs.corpus)

# Build the model

## frequencies of the 1-grams, 2-grams and 3-grams in the sample dataset
N1_Gram_Tokenizer <- function(character_vector) { NGramTokenizer(character_vector, Weka_control(min = 1, max = 1))}
N2_Gram_Tokenizer <- function(character_vector) { NGramTokenizer(character_vector, Weka_control(min = 2, max = 2))}
N3_Gram_Tokenizer <- function(character_vector) { NGramTokenizer(character_vector, Weka_control(min = 3, max = 3))}
N4_Gram_Tokenizer <- function(character_vector) { NGramTokenizer(character_vector, Weka_control(min = 4, max = 4))}
N5_Gram_Tokenizer <- function(character_vector) { NGramTokenizer(character_vector, Weka_control(min = 5, max = 5))}

### 5-gram
ALL_N5_Gram <- TermDocumentMatrix(ALL, control = list( tokenize = N5_Gram_Tokenizer))
ALL_N5_Gram_Sparse <- removeSparseTerms(ALL_N5_Gram, 0.9995)

ALL_N5_Gram_Analysis <- rollup(ALL_N5_Gram,2,na.rm=TRUE, FUN= sum)
ALL_N5_Gram_Analysis <- as.matrix(ALL_N5_Gram_Analysis)
ALL_N5_Gram_Analysis <- as.data.frame(ALL_N5_Gram_Analysis)
ALL_N5_Gram_Analysis$N3Gram <- rownames(ALL_N5_Gram_Analysis)
colnames(ALL_N5_Gram_Analysis) <- c("Count", "N5Gram")
ALL_N5_Gram_Analysis <- ALL_N5_Gram_Analysis[order(ALL_N5_Gram_Analysis$Count,decreasing = TRUE),]
ALL_N5_Gram_Analysis <- cSplit(ALL_N5_Gram_Analysis, "N5Gram", sep = " ")
    

### 4-gram
ALL_N4_Gram <- TermDocumentMatrix(ALL, control = list( tokenize = N4_Gram_Tokenizer))
ALL_N4_Gram_Sparse <- removeSparseTerms(ALL_N4_Gram, 0.9995)

ALL_N4_Gram_Analysis <- rollup(ALL_N4_Gram,2,na.rm=TRUE, FUN= sum)
ALL_N4_Gram_Analysis <- as.matrix(ALL_N4_Gram_Analysis)
ALL_N4_Gram_Analysis <- as.data.frame(ALL_N4_Gram_Analysis)
ALL_N4_Gram_Analysis$N3Gram <- rownames(ALL_N4_Gram_Analysis)
colnames(ALL_N4_Gram_Analysis) <- c("Count", "N4Gram")
ALL_N4_Gram_Analysis <- ALL_N4_Gram_Analysis[order(ALL_N4_Gram_Analysis$Count,decreasing = TRUE),]
ALL_N4_Gram_Analysis <- cSplit(ALL_N4_Gram_Analysis, "N4Gram", sep = " ")

### 3-gram
ALL_N3_Gram <- TermDocumentMatrix(ALL, control = list( tokenize = N3_Gram_Tokenizer))
ALL_N3_Gram_Sparse <- removeSparseTerms(ALL_N3_Gram, 0.9995)

ALL_N3_Gram_Analysis <- rollup(ALL_N3_Gram,2,na.rm=TRUE, FUN= sum)
ALL_N3_Gram_Analysis <- as.matrix(ALL_N3_Gram_Analysis)
ALL_N3_Gram_Analysis <- as.data.frame(ALL_N3_Gram_Analysis)
ALL_N3_Gram_Analysis$N3Gram <- rownames(ALL_N3_Gram_Analysis)
colnames(ALL_N3_Gram_Analysis) <- c("Count", "N3Gram")
ALL_N3_Gram_Analysis <- ALL_N3_Gram_Analysis[order(ALL_N3_Gram_Analysis$Count,decreasing = TRUE),]
ALL_N3_Gram_Analysis <- cSplit(ALL_N3_Gram_Analysis, "N3Gram", sep = " ")

### 2-gram
ALL_N2_Gram <- TermDocumentMatrix(ALL, control = list( tokenize = N2_Gram_Tokenizer))
ALL_N2_Gram_Sparse <- removeSparseTerms(ALL_N2_Gram, 0.9995)

ALL_N2_Gram_Analysis <- rollup(ALL_N2_Gram,2,na.rm=TRUE, FUN= sum)
ALL_N2_Gram_Analysis <- as.matrix(ALL_N2_Gram_Analysis)
ALL_N2_Gram_Analysis <- as.data.frame(ALL_N2_Gram_Analysis)
ALL_N2_Gram_Analysis$N2Gram <- rownames(ALL_N2_Gram_Analysis)
colnames(ALL_N2_Gram_Analysis) <- c("Count", "N2Gram")
ALL_N2_Gram_Analysis <- ALL_N2_Gram_Analysis[order(ALL_N2_Gram_Analysis$Count,decreasing = TRUE),]
ALL_N2_Gram_Analysis <- cSplit(ALL_N2_Gram_Analysis, "N2Gram", sep = " ")

### 1-gram
ALL_N1_Gram <- TermDocumentMatrix(ALL, control = list( tokenize = N1_Gram_Tokenizer))
ALL_N1_Gram_Sparse <- removeSparseTerms(ALL_N1_Gram, 0.9995)

ALL_N1_Gram_Analysis <- rollup(ALL_N1_Gram,2,na.rm=TRUE, FUN= sum)
ALL_N1_Gram_Analysis <- as.matrix(ALL_N1_Gram_Analysis)
ALL_N1_Gram_Analysis <- as.data.frame(ALL_N1_Gram_Analysis)
ALL_N1_Gram_Analysis$N1Gram <- rownames(ALL_N1_Gram_Analysis)
colnames(ALL_N1_Gram_Analysis) <- c("Count", "N1Gram")
ALL_N1_Gram_Analysis <- ALL_N1_Gram_Analysis[order(ALL_N1_Gram_Analysis$Count,decreasing = TRUE),]

save(ALL_N1_Gram_Analysis,ALL_N2_Gram_Analysis,ALL_N3_Gram_Analysis,ALL_N4_Gram_Analysis, file = "n-Grams.RData")

# Train the model
TRI_naiveBayes <- naiveBayes(N3Gram_3 ~ N3Gram_1 + N3Gram_2, ALL_N3_Gram_Analysis)
model <- svm(news.Tri_Gram_Analysis$news_3gram_3 ~ news.Tri_Gram_Analysis$news_3gram_1 + news.Tri_Gram_Analysis$news_3gram_2 )

# Predict
predict(TRI_naiveBayes, test_df)

source("DummyPrediction.R")