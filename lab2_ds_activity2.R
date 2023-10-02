getwd()
churn = read.csv("Churn_Train.csv", header= TRUE)

view(churn)
#is.na(<dataset>) : checks for missing data
is.na(churn)

#checks how many missing data are there
#deleting data is THE LAST CHOICE
#better of impude(ganti) data with different value, either mean
sum(is.na(churn))

#detects which row has missing value
which(is.na(churn))

#imputing(replace) missing values
churn <- churn %>%
  mutate(Total.Charges = replace(Total.Charges, is.na(Total.Charges), mean(Total.Charges, na.rm=TRUE)))
sum(is.na(churn))

sum(is.na(churn))
describe(churn)
view(churn)
normality(churn)
plot_normality(churn)
correlate(churn)
churn%>%
  correlate()%>%
  plot()
#EDA when target and predictor is categorical
categ <- target_by(churn, Payment.Method)
cat_cat <- relate(categ, Paperless.Billing)
cat_cat
summary(cat_cat)
plot(cat_cat)

#EDA when target is categorical, predictor is numerical
cat_num <- relate(categ, Monthly.Charges)
cat_num
summary(cat_num)
plot(cat_num)

#EDA when both target and predictor is numerical
num <-target_by(churn, Total.Charges)
num_num <- relate(num, Monthly.Charges)
num_num
summary(num_num)
plot(num_num)

#EDA when target is numerical, predictor is categorical
num_cat <- relate(num, Paperless.Billing)
num_cat
summary(num_cat)
plot(num_cat)

churn %>%
  eda_paged_report(target = "Payment.Method", subtitle = "Churn",output_dir = "./", output_file = "EDA_Lab2.pdf", theme = "orange")