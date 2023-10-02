#Activity 1: Data Exploration
str(Carseats)
?Carseats

#calculating descriptive statistics using describe()
#sd = standard deviation, se_mean = standard error of mean, IQR = interquartile range
#p00, p01 = percentile
describe(Carseats)
describe(Carseats, Sales, CompPrice, Income) #select columns by name
describe(Carseats, Sales:Income)#select all columns between sales and incomes(include)
describe(Carseats, -(Sales:Income))#select all columns except sales until income

#test normality - uses(or is it using...) shapiro-wilk test
#sama gak macam gua describe()
#p_value: related to null=hypothesis
normality(Carseats)
normality(Carseats, Sales, Price, Age, Education)
normality(Carseats, -Sales,- Price)

plot_normality(Carseats, Sales, CompPrice)

#correlation calculation using correlate()
correlate(Carseats)
correlate(Carseats, Sales, CompPrice, Income)
correlate(Carseats, Sales:Income)
correlate(Carseats, -(Sales:Income))

#plotting correlations
#once again, can choose variables(in the case of this dataset, its name is vars...)
Carseats%>%
  correlate() %>%
  plot()

#EDA based on target variable
categ<- target_by(Carseats, US)

#EDA when target variable is categorical, predictor is numerical
#numerical produce values, or graphs, categorical produce tables
#y can be outcome, target, dependent, response
#x can be predictor, attribute, features, independent
cat_num <- relate(categ, Sales)
cat_num
summary(cat_num)
plot(cat_num)

#ok... what if EDA when target variable is categorical, predictor is categorical?
cat_cat <- relate(categ, ShelveLoc)
cat_cat
summary(cat_cat)
plot(cat_cat)

#WAIT. there's more.
#what if EDA when target variable(y) is numerical, predictor(x) is numerical?
num <- target_by(Carseats, Sales)
num_num <- relate(num, Price)
num_num
summary(num_num)
plot(num_num)

#ok... what if EDA when target variable is numerical, predictor is categorical?
#t-value = t-test statistc
#f-statistic = ANOVA
num_cat <- relate(num, ShelveLoc)
cat_cat
summary(cat_cat)
plot(cat_cat)