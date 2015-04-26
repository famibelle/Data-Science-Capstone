ALL_N5_Gram_Analysis <- data.frame(t(rep(NA,6)))
colnames(ALL_N5_Gram_Analysis) <- c("Count",    "N5Gram_1", "N5Gram_2", "N5Gram_3", "N5Gram_4", "N5Gram_5")
ALL_N5_Gram_Analysis <- ALL_N5_Gram_Analysis[-1,]

ALL_N4_Gram_Analysis <- data.frame(t(rep(NA,5)))
colnames(ALL_N4_Gram_Analysis) <- c("Count",    "N4Gram_1", "N4Gram_2", "N4Gram_3", "N4Gram_4")
ALL_N4_Gram_Analysis <- ALL_N4_Gram_Analysis[-1,]

ALL_N3_Gram_Analysis <- data.frame(t(rep(NA,4)))
colnames(ALL_N3_Gram_Analysis) <- c("Count",    "N3Gram_1", "N3Gram_2", "N3Gram_3")
ALL_N3_Gram_Analysis <- ALL_N3_Gram_Analysis[-1,]

ALL_N2_Gram_Analysis <- data.frame(t(rep(NA,3)))
colnames(ALL_N2_Gram_Analysis) <- c("Count",    "N2Gram_1", "N2Gram_2")
ALL_N2_Gram_Analysis <- ALL_N2_Gram_Analysis[-1,]

ALL_N1_Gram_Analysis <- data.frame(t(rep(NA,2)))
colnames(ALL_N1_Gram_Analysis) <- c("Count",  "N1Gram")
ALL_N1_Gram_Analysis <- ALL_N1_Gram_Analysis[-1,]

twitter_blogs_news <- c(twitter, blogs, news)
chunks<- matrix(1:length(twitter_blogs_news), ncol = 10000, nrow = (length(twitter_blogs_news)/10000), byrow = TRUE)

for (i in 1:nrow(chunks)) {
    print(paste("working on chunk", i, "over", nrow(chunks)))
    ALL  <- Create_Corpus(twitter_blogs_news[(chunks[i,])])
    
    # looking from N5_Grams
    print(paste("    looking for N5_Gram, chunk", i, "/", nrow(chunks)))
    N5_Gram <- TermDocumentMatrix(ALL, control = list( tokenize = N5_Gram_Tokenizer))        
    N5_Gram_Analysis <- rollup(N5_Gram,2,na.rm=TRUE, FUN= sum)
    N5_Gram_Analysis <- as.matrix(N5_Gram_Analysis)
    N5_Gram_Analysis <- as.data.frame(N5_Gram_Analysis)
    N5_Gram_Analysis$N5Gram <- rownames(N5_Gram_Analysis)
    colnames(N5_Gram_Analysis) <- c("Count", "N5Gram")
    N5_Gram_Analysis <- as.data.table(N5_Gram_Analysis)
    N5_Gram_Analysis <- N5_Gram_Analysis[order(N5_Gram_Analysis$Count,decreasing = TRUE),]
    N5_Gram_Analysis <- cSplit(N5_Gram_Analysis, "N5Gram", sep = " ")
    ALL_N5_Gram_Analysis <- rbind(ALL_N5_Gram_Analysis, N5_Gram_Analysis)    
    
    # looking from N4_Grams
    print(paste("    looking for N4_Gram, chunk", i, "/", nrow(chunks)))
    N4_Gram <- TermDocumentMatrix(ALL, control = list( tokenize = N4_Gram_Tokenizer))        
    N4_Gram_Analysis <- rollup(N4_Gram,2,na.rm=TRUE, FUN= sum)
    N4_Gram_Analysis <- as.matrix(N4_Gram_Analysis)
    N4_Gram_Analysis <- as.data.frame(N4_Gram_Analysis)
    N4_Gram_Analysis$N5Gram <- rownames(N4_Gram_Analysis)
    colnames(N4_Gram_Analysis) <- c("Count", "N4Gram")
    N4_Gram_Analysis <- as.data.table(N4_Gram_Analysis)
    N4_Gram_Analysis <- N4_Gram_Analysis[order(N4_Gram_Analysis$Count,decreasing = TRUE),]
    N4_Gram_Analysis <- cSplit(N4_Gram_Analysis, "N4Gram", sep = " ")
    ALL_N4_Gram_Analysis <- rbind(ALL_N4_Gram_Analysis, N4_Gram_Analysis)    
    
    # looking from N3_Grams
    print(paste("    looking for N3_Gram, chunk", i, "/", nrow(chunks)))
    N3_Gram <- TermDocumentMatrix(ALL, control = list( tokenize = N3_Gram_Tokenizer))        
    N3_Gram_Analysis <- rollup(N3_Gram,2,na.rm=TRUE, FUN= sum)
    N3_Gram_Analysis <- as.matrix(N3_Gram_Analysis)
    N3_Gram_Analysis <- as.data.frame(N3_Gram_Analysis)
    N3_Gram_Analysis$N5Gram <- rownames(N3_Gram_Analysis)
    colnames(N3_Gram_Analysis) <- c("Count", "N3Gram")
    N3_Gram_Analysis <- as.data.table(N3_Gram_Analysis)
    N3_Gram_Analysis <- N3_Gram_Analysis[order(N3_Gram_Analysis$Count,decreasing = TRUE),]
    N3_Gram_Analysis <- cSplit(N3_Gram_Analysis, "N3Gram", sep = " ")
    ALL_N3_Gram_Analysis <- rbind(ALL_N3_Gram_Analysis, N3_Gram_Analysis)    
    
    # looking from N2_Grams
    print(paste("    looking for N2_Gram, chunk", i, "/", nrow(chunks)))
    N2_Gram <- TermDocumentMatrix(ALL, control = list( tokenize = N2_Gram_Tokenizer))        
    N2_Gram_Analysis <- rollup(N2_Gram,2,na.rm=TRUE, FUN= sum)
    N2_Gram_Analysis <- as.matrix(N2_Gram_Analysis)
    N2_Gram_Analysis <- as.data.frame(N2_Gram_Analysis)
    N2_Gram_Analysis$N5Gram <- rownames(N2_Gram_Analysis)
    colnames(N2_Gram_Analysis) <- c("Count", "N2Gram")
    N2_Gram_Analysis <- as.data.table(N2_Gram_Analysis)
    N2_Gram_Analysis <- N2_Gram_Analysis[order(N2_Gram_Analysis$Count,decreasing = TRUE),]
    N2_Gram_Analysis <- cSplit(N2_Gram_Analysis, "N2Gram", sep = " ")
    ALL_N2_Gram_Analysis <- rbind(ALL_N2_Gram_Analysis, N2_Gram_Analysis)    
    
    # looking from N1_Grams
    print(paste("    looking for N1_Gram, chunk", i, "/", nrow(chunks)))
    N1_Gram <- TermDocumentMatrix(ALL, control = list( tokenize = N1_Gram_Tokenizer))        
    N1_Gram_Analysis <- rollup(N1_Gram,2,na.rm=TRUE, FUN= sum)
    N1_Gram_Analysis <- as.matrix(N1_Gram_Analysis)
    N1_Gram_Analysis <- as.data.frame(N1_Gram_Analysis)
    N1_Gram_Analysis$N5Gram <- rownames(N1_Gram_Analysis)
    colnames(N1_Gram_Analysis) <- c("Count", "N1Gram")
    N1_Gram_Analysis <- as.data.table(N1_Gram_Analysis)
    N1_Gram_Analysis <- N1_Gram_Analysis[order(N1_Gram_Analysis$Count,decreasing = TRUE),]
    ALL_N1_Gram_Analysis <- rbind(ALL_N1_Gram_Analysis, N1_Gram_Analysis)
    
    print(paste("N1_Grams size: ", dim(ALL_N1_Gram_Analysis)))
    print(paste("N2_Grams size: ", dim(ALL_N2_Gram_Analysis)))
    print(paste("N3_Grams size: ", dim(ALL_N3_Gram_Analysis)))
    print(paste("N4_Grams size: ", dim(ALL_N4_Gram_Analysis)))
    print(paste("N5_Grams size: ", dim(ALL_N5_Gram_Analysis)))
}
