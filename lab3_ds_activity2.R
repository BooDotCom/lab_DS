getwd()
churn2 = read.csv("Churnn_Train.csv", header = TRUE)

is.na(churn2)#checks for missing value
sum(is.na(churn2$Total.Charges))#returns total number of rows with missing value

which(is.na(churn2))#returns the rows with missing value

churn_numeric <- churn2 %>%
  select(Monthly.Charges,Tenure,Total.Charges)

md.pattern(churn_numeric)

#creates data frame that consist of columns of various total charges in various imputed forms
valuemiceforest_imputed <- data.frame(
  original = churn2$Total.Charges,#original dataset
  imputed_zero = replace(churn2$Total.Charges, is.na(churn2$Total.Charges), 0),#impute missing value with mean
  imputed_mean = replace(churn2$Total.Charges, is.na(churn2$Total.Charges), mean(churn2$Total.Charges, na.rm = TRUE)),#impute missing value with mean
  imputed_median = replace(churn2$Total.Charges, is.na(churn2$Total.Charges), median(churn2$Total.Charges, na.rm = TRUE)),#impute missing value with median
  imputed_pmm = complete(mice(churn_numeric, method = "pmm"))$Total.Charges,#impute missing value using pmm
  imputed_cart = complete(mice(churn_numeric, method = "cart"))$Total.Charges,#impute missing value using CART
  imputed_lasso = complete(mice(churn_numeric, method = "lasso.norm"))$Total.Charges,#impute missing value using lasso
  imputed_missForest = missForest(churn_numeric)$ximp$Total.Charges#impute missing value using missForest
)

#creates histograms for each imputation
h1 <- ggplot(valuemiceforest_imputed, aes(x = original)) +#line 27-30: original dataset
  geom_histogram(fill = "#ad1538", color = "#000000", position = "identity") +
  ggtitle("Original distribution") +
  theme_classic()
h2 <- ggplot(valuemiceforest_imputed, aes(x = imputed_zero)) +#line 31-34: zero-imputed dataset
  geom_histogram(fill = "#15ad4f", color = "#000000", position = "identity") +
  ggtitle("Zero-imputed distribution") +
  theme_classic()
h3 <- ggplot(valuemiceforest_imputed, aes(x = imputed_mean)) +#line 35-38: mean-imputed dataset
  geom_histogram(fill = "#1543ad", color = "#000000", position = "identity") +
  ggtitle("Mean-imputed distribution") +
  theme_classic()
h4 <- ggplot(valuemiceforest_imputed, aes(x = imputed_median)) +#line 39-42: median-imputed dataset
  geom_histogram(fill = "#ad8415", color = "#000000", position = "identity") +
  ggtitle("Median-imputed distribution") +
  theme_classic()
h5 <- ggplot(valuemiceforest_imputed, aes(x = imputed_pmm)) +#line 43-46: pmm-imputed dataset
  geom_histogram(fill = "#ff9933", color = "#000000", position = "identity") +
  ggtitle("pmm-imputed distribution") +
  theme_classic()
h6 <- ggplot(valuemiceforest_imputed, aes(x = imputed_cart)) +#line 47-50: CART imputed dataset
  geom_histogram(fill = "#ffff00", color = "#000000", position = "identity") +
  ggtitle("CART-imputed distribution") +
  theme_classic()
h7 <- ggplot(valuemiceforest_imputed, aes(x = imputed_lasso)) +#line 51-54: lasso imputed dataset
  geom_histogram(fill = "#ff33ff", color = "#000000", position = "identity") +
  ggtitle("Lasso-imputed distribution") +
  theme_classic()
h8 <- ggplot(valuemiceforest_imputed, aes(x = imputed_missForest)) +#line 55-58: missForest-imputed dataset
  geom_histogram(fill = "#808080", color = "#000000", position = "identity") +
  ggtitle("missForest-imputed distribution") +
  theme_classic()
plot_grid(h1, h2, h3, h4, nrow = 2, ncol = 2)#plots a 2x2 grid of histograms h1 until h4. can be replaced with h5, h6, h7, h8 to plot them instead
