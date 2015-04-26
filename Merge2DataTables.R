# merge N-grams datatable

merge2datatables <- function(
    A = data.table(c(1,5,6), c("i", "i", "you"),   c("love", "eat","put"), c("you", "cheese","perfume")),
    B = data.table(c(3,7,9), c("rt", "i", "give"), c("the", "love","me"),  c("king", "you","money"))
) {
    print("Merging data tables...")
    toto <- merge(A, B, by = colnames(A)[-1], all.x = TRUE, all.y = TRUE)
    toto[is.na(toto)] <- 0
    C <- as.data.table(transform(toto, Count = rowSums(subset(toto, select = c((ncol(toto)-1), ncol(toto))))))
    newOrder <- c(colnames(C)[ncol(C)], colnames(C)[2:ncol(C)-1])
    setcolorder(C,neworder = newOrder)
    columnstokeep <- colnames(C)[1:(ncol(C)-2)]
    subset(C,select = columnstokeep)
}