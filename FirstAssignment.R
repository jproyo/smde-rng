
fibflagged <- read.table("/Users/juan/Projects/upc/master/smde/rng-j/sample.csv", 
    header = FALSE, sep = ",", na.strings = "NA", dec = ".", strip.white = TRUE)

fibflagged$bins <- with(fibflagged, binVariable(V1, bins = 10, method = "intervals", 
    labels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10")))

fibflagged_trans <- as.data.frame(with(fibflagged, table(bins)))

distribution_r <- as.data.frame(matrix(runif(500 * 1, min = 0, max = 1), ncol = 1))
rownames(distribution_r) <- paste("sample", 1:500, sep = "")
colnames(distribution_r) <- "obs"
distribution_r <- within(distribution_r, {
    mean <- rowMeans(distribution_r[, 1:1])
})

distribution_r$bins <- with(distribution_r, binVariable(obs, bins = 10, method = "intervals", 
    labels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10")))


distribution_r_trans <- as.data.frame(with(distribution_r, table(frequency)))

merge_fibflagged_r_comb <- merge(fibflagged_trans, distribution_r_trans, all = TRUE, 
    by = "row.names")
rownames(merge_fibflagged_r_comb) <- merge_fibflagged_r_comb$Row.names
merge_fibflagged_r_comb$Row.names <- NULL



to_be_test_chi_r_fibflagged <- within(merge_fibflagged_r_comb, {
	bins <- NULL
	frequency <- NULL
})

test <- chisq.test(to_be_test_chi_r_fibflagged, correct = FALSE)

