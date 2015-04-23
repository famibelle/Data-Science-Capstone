load(file = "n-Grams.RData")

DummyPrediction <- function(sentence = "I love") {
    sentence <- str_trim(sentence)
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
    result4$Probability <- result4$Count/sum(result4$Count)
    
    Query <- ALL_N3_Gram_Analysis$N3Gram_1 == sentence[2] &
        ALL_N3_Gram_Analysis$N3Gram_2 == sentence[3]
    result3 <- as.data.frame(ALL_N3_Gram_Analysis[Query])
    result3$Probability <- result3$Count/sum(result3$Count)
    
    Query <- ALL_N2_Gram_Analysis$N2Gram_1 == sentence[3]
    result2 <- as.data.frame(ALL_N2_Gram_Analysis[Query])
    result2$Probability <- result2$Count/sum(result2$Count)
    
    if (nrow(result4) != 0 ) {
        return(result4)
    }
    
    if (nrow(result3) != 0 ) {
        return(result3)
    }
    
    if (nrow(result2) != 0 ) {
        return(result2)
    }
    
    return(as.data.frame("Not found..."))
}
