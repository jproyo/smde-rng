
fibflagged <- read.table("/Users/juan/Projects/upc/master/smde/smde-rng/500_random_numbers.csv", 
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

fibflagged$morebins <- with(fibflagged, binVariable(V1, bins = 20, method = "intervals", 
    labels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20")))

distribution_r$morebins <- with(distribution_r, binVariable(obs, bins = 20, method = "intervals", 
    labels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20")))

fibflagged_trans_20bins <- as.data.frame(with(fibflagged, table(morebins)))

distribution_r_trans_20bins <- as.data.frame(with(distribution_r, table(morebins)))


merge_fibflagged_r_comb_20bins <- merge(fibflagged_trans_20bins, distribution_r_trans_20bins, all = TRUE, 
    by = "row.names")
rownames(merge_fibflagged_r_comb_20bins) <- merge_fibflagged_r_comb_20bins$Row.names
merge_fibflagged_r_comb_20bins$Row.names <- NULL



test_chi_r_fibflagged_20bins <- within(merge_fibflagged_r_comb_20bins, {
	morebins.x <- NULL
	morebins.y <- NULL
})


chi_20bins <- chisq.test(test_chi_r_fibflagged_20bins, correct = FALSE)



Norm_m0_s1 <- read.table("/Users/juan/Projects/upc/master/smde/smde-rng/1500_normal_mu_0_sigma_1.csv", 
    header = FALSE, sep = ",", na.strings = "NA", dec = ".", strip.white = TRUE)

Norm_m10_s1 <- read.table("/Users/juan/Projects/upc/master/smde/smde-rng/1500_normal_mu_10_sigma_1.csv", 
    header = FALSE, sep = ",", na.strings = "NA", dec = ".", strip.white = TRUE)

Norm_m0_s1_2 <- read.table("/Users/juan/Projects/upc/master/smde/smde-rng/1500_normal_mu_0_sigma_1_2.csv", 
    header = FALSE, sep = ",", na.strings = "NA", dec = ".", strip.white = TRUE)


Norm_v1n=data.frame(x1=Norm_m0_s1, x2="v1")
Norm_v2n=data.frame(x1=Norm_m10_s1, x2="v2")
Norm_v3n=data.frame(x1=Norm_m0_s1_2, x2="v3")
data=mergeRows(Norm_v1n, Norm_v2n, common.only=FALSE)
data=mergeRows(as.data.frame(data), Norm_v3n, common.only=FALSE)

AnovaModel.1 <- aov(V1 ~ x2, data=data)
summary(AnovaModel.1)
Boxplot(V1~x2, data=data, id.method="y")
	

library("lmtest", lib.loc="~/R/win-library/3.0")

#The observations within each sample must be independent.
#Durbin Watson 
library("lmtest", lib.loc="~/R/win-library/3.0")
dwtest(AnovaModel.1, alternative ="two.sided")
#The populations from which the samples are selected must be normal.
#Shapiro test
shapiro.test(residuals(AnovaModel.1))
#The populations from which the samples are selected must have equal variances (homogeneity of variance)
#Breusch Pagan test
lmtest::bptest(AnovaModel.1)


wine_data <- data(wine, package = "FactoMineR")

wine_data_soil_odori <- wine[,c("Odor.Intensity.before.shaking", "Soil")]
colnames(wine_data_soil_odori) <- c("odori", "soil")
AnovaModel_soil_odori <- aov(odori ~ soil, data=wine_data_soil_odori)
summary(AnovaModel_soil_odori)


wine_data_soil_aromaq <- wine[,c("Aroma.quality.before.shaking", "Soil")]
colnames(wine_data_soil_aromaq) <- c("aromaq", "soil")
AnovaModel_soil_aromaq <- aov(aromaq ~ soil, data=wine_data_soil_aromaq)
summary(AnovaModel_soil_aromaq)


wine_data_label_odori <- wine[,c("Odor.Intensity.before.shaking", "Label")]
colnames(wine_data_label_odori) <- c("odori", "label")
AnovaModel_label_odori <- aov(odori ~ label, data=wine_data_label_odori)
summary(AnovaModel_label_odori)


wine_data_label_aromaq <- wine[,c("Aroma.quality.before.shaking", "Label")]
colnames(wine_data_label_aromaq) <- c("aromaq", "label")
AnovaModel_label_aromaq <- aov(aromaq ~ label, data=wine_data_label_aromaq)
summary(AnovaModel_label_aromaq)


