df<- data.frame(Product = c('A', 'B', 'C', 'D', 'E'), Price = c(10,20,NA,50,90))
df

#imputing missing value with 0
df$Price[is.na(df$Price)] <- 0
df

#imputing missing value with 1
df$Price[is.na(df$Price)] <- mean(df$Price, na.rm =TRUE)
df

#imputing missing value with median

df$Price[is.na(df$Price)] <- median(df$Price, na.rm =TRUE)
df

library(titanic)
summary(titanic_train)

titanic_train$Age

ggplot(titanic_train, aes(Age))+ #aes is mapped to Age
  geom_histogram(color = "#000000", fill = "#0099F8") +
  theme_classic()+
  theme(plot.title = element_text(size=18))

#calc_mode <- function(x){
#  distinct_values <- unique(x)
#  distinct_tabulate <-tabulate(match(x, distinct_values))
#  distinct_values[which.max(distinct_tabulate)]
#}
#calc_mode(y)
#y<- data.frame(Num = '1,2,3,3')

value_imputed<-data.frame(original = titanic_train$Age, imputed_zero = replace(titanic_train$Age, is.na(titanic_train$Age), 0)
                          ,imputed_mean = replace(titanic_train$Age, is.na(titanic_train$Age), mean(titanic_train$Age, na.rm=TRUE))
                          ,imputed_median = replace(titanic_train$Age, is.na(titanic_train$Age), median(titanic_train$Age, na.rm=TRUE))
                          #,imputed_mode = replace(titanic_train$Age, is.na(titanic_train$Age), calc_mode(titanic_train$Age))
                          )

value_imputed

titanic_numeric <- titanic_train %>%
  select(Survived, Pclass, SibSp, Parch, Age)
md.pattern(titanic_numeric)

missForest_imputed <- data.frame(
  original = titanic_numeric$Age,
  imputed_missForest = missForest(titanic_numeric)$ximp$Age
)

h1 <- ggplot(value_imputed, aes(x = original)) +
  geom_histogram(fill = "#ad1538", color = "#000000", position =
                   "identity") +
  ggtitle("Original distribution") +
  theme_classic()
h2 <- ggplot(value_imputed, aes(x = imputed_zero)) +
  geom_histogram(fill = "#15ad4f", color = "#000000", position =
                   "identity") +
  ggtitle("Zero-imputed distribution") +
  theme_classic()
h3 <- ggplot(value_imputed, aes(x = imputed_mean)) +
  geom_histogram(fill = "#1543ad", color = "#000000", position =
                   "identity") +
  ggtitle("Mean-imputed distribution") +
  theme_classic()
h4 <- ggplot(value_imputed, aes(x = imputed_median)) +
  geom_histogram(fill = "#ad8415", color = "#000000", position =
                   "identity") +
  ggtitle("Median-imputed distribution") +
  theme_classic()
plot_grid(h1, h2, h3, h4, nrow = 2, ncol = 2)