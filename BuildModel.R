library(splitstackshape)
library(e1071)
library(tm)
library(RWeka)
library(reshape2)
library(stringr)


# Loading the data
con <- file("en_US.twitter.txt", "r")
twitter <- readLines(con, encoding="UTF-8")
close(con)

con <- file("en_US.blogs.txt", "r")
blogs <- readLines(con, encoding="UTF-8")
close(con)

con <- file("en_US.news.txt", "r")
news <- readLines(con, encoding="UTF-8")
close(con)
rm(con)

# Clean the data
## Create a corpus
Create_Corpus <- function(sample) {
    Corpus <- VCorpus(VectorSource(sample), readerControl = list(language = "en"))
    Corpus <- tm_map(Corpus, removePunctuation)
    Corpus <- tm_map(Corpus, stripWhitespace)
    Corpus <- tm_map(Corpus, removeNumbers)
    Corpus <- tm_map(Corpus, content_transformer(tolower))
#   Corpus <- tm_map(Corpus, removeWords, Profanities)
#   Corpus <- tm_map(Corpus, stemDocument) # Stem document
    return(Corpus)
}

twitter.corpus  <- Create_Corpus(twitter); rm(twitter)
blogs.corpus    <- Create_Corpus(blogs); rm(blogs)
news.corpus     <- Create_Corpus(news); rm(news)

## frequencies of the 1-grams, 2-grams and 3-grams in the sample dataset
N1_Gram_Tokenizer <- function(character_vector) { NGramTokenizer(character_vector, Weka_control(min = 1, max = 1))}
N2_Gram_Tokenizer <- function(character_vector) { NGramTokenizer(character_vector, Weka_control(min = 2, max = 2))}
N3_Gram_Tokenizer <- function(character_vector) { NGramTokenizer(character_vector, Weka_control(min = 3, max = 3))}
N4_Gram_Tokenizer <- function(character_vector) { NGramTokenizer(character_vector, Weka_control(min = 4, max = 4))}

ALL <- c(twitter.corpus, news.corpus, blogs.corpus)

### 4-gram
ALL_N4_Gram <- TermDocumentMatrix(ALL, control = list( tokenize = N4_Gram_Tokenizer))
ALL_N4_Gram_Sparse <- removeSparseTerms(ALL_N4_Gram, 0.9995)

ALL_N4_Gram_Analysis <- rowSums(as.matrix(ALL_N4_Gram_Sparse))
ALL_N4_Gram_Analysis <- sort(ALL_N4_Gram_Analysis, decreasing = TRUE)
ALL_N4_Gram_Analysis <- as.data.frame(ALL_N4_Gram_Analysis)
ALL_N4_Gram_Analysis$N3Gram <- rownames(ALL_N4_Gram_Analysis)
colnames(ALL_N4_Gram_Analysis) <- c("count", "N4Gram")


### 3-gram
ALL_N3_Gram <- TermDocumentMatrix(ALL, control = list( tokenize = N3_Gram_Tokenizer))
ALL_N3_Gram_Sparse <- removeSparseTerms(ALL_N3_Gram, 0.9995)

ALL_N3_Gram_Analysis <- rowSums(as.matrix(ALL_N3_Gram_Sparse))
ALL_N3_Gram_Analysis <- sort(ALL_N3_Gram_Analysis, decreasing = TRUE)
ALL_N3_Gram_Analysis <- as.data.frame(ALL_N3_Gram_Analysis)
ALL_N3_Gram_Analysis$N3Gram <- rownames(ALL_N3_Gram_Analysis)
ALL_N3_Gram_Analysis <- ALL_N3_Gram_Analysis[,c(2,1)]
colnames(ALL_N3_Gram_Analysis) <- c("N3Gram", "count")

### 2-gram
ALL_N2_Gram <- TermDocumentMatrix(ALL, control = list( tokenize = N2_Gram_Tokenizer))
ALL_N2_Gram_Sparse <- removeSparseTerms(ALL_N2_Gram, 0.9995)

ALL_N2_Gram_Analysis <- rowSums(as.matrix(ALL_N2_Gram_Sparse))
ALL_N2_Gram_Analysis <- sort(ALL_N2_Gram_Analysis, decreasing = TRUE)
ALL_N2_Gram_Analysis <- as.data.frame(ALL_N2_Gram_Analysis)
ALL_N2_Gram_Analysis$N2Gram <- rownames(ALL_N2_Gram_Analysis)
colnames(ALL_N2_Gram_Analysis) <- c( "count", "N2Gram")

### 1-gram
ALL_N1_Gram <- TermDocumentMatrix(ALL, control = list( tokenize = N1_Gram_Tokenizer))
ALL_N1_Gram_Sparse <- removeSparseTerms(ALL_N1_Gram, 0.9995)

ALL_N1_Gram_Analysis <- rowSums(as.matrix(ALL_N1_Gram_Sparse))
ALL_N1_Gram_Analysis <- sort(ALL_N1_Gram_Analysis, decreasing = TRUE)
ALL_N1_Gram_Analysis <- as.data.frame(ALL_N1_Gram_Analysis)
ALL_N1_Gram_Analysis$N2Gram <- rownames(ALL_N1_Gram_Analysis)
colnames(ALL_N1_Gram_Analysis) <- c( "count", "N1Gram")


# Build the model
ALL_N4_Gram_Analysis <- cSplit(ALL_N4_Gram_Analysis, "N4Gram", sep = " ")
ALL_N3_Gram_Analysis <- cSplit(ALL_N3_Gram_Analysis, "N3Gram", sep = " ")
ALL_N2_Gram_Analysis <- cSplit(ALL_N2_Gram_Analysis, "N2Gram", sep = " ")

# Train the model
TRI_naiveBayes <- naiveBayes(N3Gram_3 ~ N3Gram_1 + N3Gram_2, ALL_N3_Gram_Analysis)
model <- svm(news.Tri_Gram_Analysis$news_3gram_3 ~ news.Tri_Gram_Analysis$news_3gram_1 + news.Tri_Gram_Analysis$news_3gram_2 )

# Predict
predict(TRI_naiveBayes, test_df)

DummyPrediction <- function(sentence = "It is") {
    sentence <- tolower(sentence)
    sentence <- removePunctuation(sentence)
    sentence <- removeNumbers(sentence)
    sentence <- stripWhitespace(sentence)
    wordcount <- length(unlist(strsplit(sentence, split = " ")))
    if (wordcount > 2) {
        sentence <- word(sentence,c(-3, -2, -1))
    }
    
    if (wordcount == 2) {
        sentence <- c("dummy", word(sentence,-2),word(sentence,-1))
    }
    
    if (wordcount == 1) {
        sentence <- c("dummy", "dummy", sentence)
    }
    
    Query <- ALL_N4_Gram_Analysis$N4Gram_1 == sentence[1] & 
        ALL_N4_Gram_Analysis$N4Gram_2 == sentence[2] &
        ALL_N4_Gram_Analysis$N4Gram_3 == sentence[3]
    result4 <- as.data.frame(ALL_N4_Gram_Analysis[Query])
    
    Query <- ALL_N3_Gram_Analysis$N3Gram_1 == sentence[2] &
        ALL_N3_Gram_Analysis$N3Gram_2 == sentence[3]
    result3 <- as.data.frame(ALL_N3_Gram_Analysis[Query])
    
    Query <- ALL_N2_Gram_Analysis$N2Gram_1 == sentence[3]
    result2 <- as.data.frame(ALL_N2_Gram_Analysis[Query])
    
    if (nrow(result4) != 0 ) {
        return(as.character(result4$N4Gram_4[1]))
    }
    
    if (nrow(result3) != 0 ) {
        return(as.character(result3$N3Gram_3[1]))
    }
    
    if (nrow(result2) != 0 ) {
        return(as.character(result2$N2Gram_2[1]))
    }
    
    return("Not found...")
}
