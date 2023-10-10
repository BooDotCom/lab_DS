getwd()
churn2 = read.csv("Churnn_Train.csv", header = TRUE)

is.na(churn2)
sum(is.na(churn2$Total.Charges))

which(is.na(churn2))

churn_numeric <- churn2 %>%
  select(Monthly.Charges,Tenure,Total.Charges)

md.pattern(churn_numeric)

#creates data frame that consist of columns of various total charges in various imputed forms
valuemiceforest_imputed <- data.frame(
  original = churn2$Total.Charges,
  imputed_zero = replace(churn2$Total.Charges, is.na(churn2$Total.Charges), 0),
  imputed_mean = replace(churn2$Total.Charges, is.na(churn2$Total.Charges), mean(churn2$Total.Charges, na.rm = TRUE)),
  imputed_median = replace(churn2$Total.Charges, is.na(churn2$Total.Charges), median(churn2$Total.Charges, na.rm = TRUE)),
  imputed_pmm = complete(mice(churn_numeric, method = "pmm"))$Total.Charges,
  imputed_cart = complete(mice(churn_numeric, method = "cart"))$Total.Charges,
  imputed_lasso = complete(mice(churn_numeric, method = "lasso.norm"))$Total.Charges,
  imputed_missForest = missForest(churn_numeric)$ximp$Total.Charges
)

h1 <- ggplot(valuemiceforest_imputed, aes(x = original)) +
  geom_histogram(fill = "#ad1538", color = "#000000", position = "identity") +
  ggtitle("Original distribution") +
  theme_classic()
h2 <- ggplot(valuemiceforest_imputed, aes(x = imputed_zero)) +
  geom_histogram(fill = "#15ad4f", color = "#000000", position = "identity") +
  ggtitle("Zero-imputed distribution") +
  theme_classic()
h3 <- ggplot(valuemiceforest_imputed, aes(x = imputed_mean)) +
  geom_histogram(fill = "#1543ad", color = "#000000", position = "identity") +
  ggtitle("Mean-imputed distribution") +
  theme_classic()
h4 <- ggplot(valuemiceforest_imputed, aes(x = imputed_median)) +
  geom_histogram(fill = "#ad8415", color = "#000000", position = "identity") +
  ggtitle("Median-imputed distribution") +
  theme_classic()
h5 <- ggplot(valuemiceforest_imputed, aes(x = imputed_pmm)) +
  geom_histogram(fill = "#ff9933", color = "#000000", position = "identity") +
  ggtitle("pmm-imputed distribution") +
  theme_classic()
h6 <- ggplot(valuemiceforest_imputed, aes(x = imputed_cart)) +
  geom_histogram(fill = "#ffff00", color = "#000000", position = "identity") +
  ggtitle("CART-imputed distribution") +
  theme_classic()
h7 <- ggplot(valuemiceforest_imputed, aes(x = imputed_lasso)) +
  geom_histogram(fill = "#ff33ff", color = "#000000", position = "identity") +
  ggtitle("Lasso-imputed distribution") +
  theme_classic()
h8 <- ggplot(valuemiceforest_imputed, aes(x = imputed_missForest)) +
  geom_histogram(fill = "#808080", color = "#000000", position = "identity") +
  ggtitle("missForest-imputed distribution") +
  theme_classic()
plot_grid(h1, h2, h3, h4, nrow = 2, ncol = 2)
