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
