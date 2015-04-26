## n-Grams tokenizer, frequencies of the 1-grams, 2-grams and 3-grams in the sample dataset
N1_Gram_Tokenizer <- function(character_vector) { NGramTokenizer(character_vector, Weka_control(min = 1, max = 1))}
N2_Gram_Tokenizer <- function(character_vector) { NGramTokenizer(character_vector, Weka_control(min = 2, max = 2))}
N3_Gram_Tokenizer <- function(character_vector) { NGramTokenizer(character_vector, Weka_control(min = 3, max = 3))}
N4_Gram_Tokenizer <- function(character_vector) { NGramTokenizer(character_vector, Weka_control(min = 4, max = 4))}
N5_Gram_Tokenizer <- function(character_vector) { NGramTokenizer(character_vector, Weka_control(min = 5, max = 5))}
