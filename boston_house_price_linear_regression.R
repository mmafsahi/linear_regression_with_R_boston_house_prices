#===================================================================
# ISM645 / IAF601   Principle of Predictive Analytics
# Linear Regression
#  November 2, 11:59 pm
#===================================================================


library(tidyverse)


# Import the csv file (Boston_house_prices.csv) and explore it.
#====================== Write R code HERE ==========================
df <- read.csv('Boston_house_prices.csv')
head(df,4)
summary(df)
str(df)

#===================================================================



#======= Question 1 (1 Point) =======
# Q1-1. Spread the data out to multiple columns, with the shop type (Starbucks vs. Dunkin Donuts) being the key column and the number of shops being the value column.
# Q1-2. Delete observations that contain at least one missing value or invalid value (e.g., negative income).

#====================== Write R code HERE ==========================

df_spread <- df %>% 
  spread(key=shop_type,value=num_shops)
head(df_spread)

any(is.na(df_spread))

df_spread <- df_spread %>% 
  filter(median_income >0) %>%
  drop_na()

if ( !any(is.na(df_spread))) {
  
  print(paste0('The data has spreaded and cleaned without NAs and also the negative median_incom omitted!'))
  
}

head(df_spread)

#===================================================================



#======= Question 2 (1 Point) =======
# Q2-1. Create a scatter plot to examine the relationship between house prices and Starbucks.
# Q2-2. Repeat Q2-1 for Dunkin Donuts.

#====================== Write R code HERE ==========================

library(plotly)
pl1 <- ggplot(df_spread,aes(x=starbucks,y=house_price_index))+
  geom_point(color='blue')+
  geom_smooth(se=F,method = 'lm',color='red')+
  theme_bw()

ggplotly(pl1)

pl2 <- ggplot(df_spread,aes(x=dunkin_donuts,y=house_price_index))+
  geom_point(color='blue')+
  geom_smooth(se=F,method = 'lm',color='red')+
  theme_bw()
ggplotly(pl2)

#===================================================================



#======= Question 3 (1 Point) =======
# Q3. Build a linear regression model to predict house prices based on the number of Starbucks and Dunkin Donuts.

#====================== Write R code HERE ==========================

model <- lm(house_price_index ~ dunkin_donuts + starbucks, data=df_spread)

print(model)
summary(model)

model2 <- lm(house_price_index ~ .,data=df_spread)
summary(model2)

#===================================================================


#======= Question 4 (2 Point) =======
# One might argue that neighborhoods where Starbucks are located are relatively rich. 
# We want to examine if Starbucks still has a predictive power for house prices, even after controlling for household incomes and population.
# Q4-1. Create new variables by taking log to median_income and population.
# Q4-2. Build a linear regression model to predict house prices based on the number of Starbucks and Dunkin Donuts as well as (logarithm of) household incomes and population.

#====================== Write R code HERE ==========================

df_spread_log_transformed <-df_spread %>%
  mutate(log_median_income=log(median_income),log_population=log(population))


modelLog <-lm(house_price_index ~ log_median_income + log_population + starbucks +dunkin_donuts,data=df_spread_log_transformed)
summary(modelLog)



# Q4-3. Do you think considering median income and population improves the linear regression model? 
#       Write your opinion briefly by commenting (#).

#### Response -----------------------> 

# YES, the two predictors of median_incom and population have much effect on the response house_price variable. 
# As their significance or p-values suggest that they are higher peredictor than the starbucks and dunkin_donuts. 
# That's why the last model did a better prediction and improvements in adjusted R-squared with the 34%.




#===================================================================



#======= Question 5 (2 Point) =======
# The dynamics of house prices might vary across counties.
# Q5-1. Separate the plot resulting from Question 2 by county.
# Q5-2. Add the county variable to the previous linear regression model (from Question 4).

#====================== Write R code HERE ==========================
pl3 <- ggplot(df_spread,aes(x=starbucks,y=house_price_index,fill=county))+
  geom_point(color='blue')+
  geom_smooth(se=F,method = 'lm',color='red')+
  theme_bw()

ggplotly(pl3)

pl4 <- ggplot(df_spread,aes(x=dunkin_donuts,y=house_price_index,fill=county))+
  geom_point(color='blue')+
  geom_smooth(se=F,method = 'lm',color='red')+
  theme_bw()
ggplotly(pl4)

modelLogPlusCounty <-lm(house_price_index ~ log_median_income + log_population + starbucks +dunkin_donuts + county,data=df_spread_log_transformed)
summary(modelLogPlusCounty)

#===================================================================



#======= Question 6 (2 Point) =======
# Q6-1. Do you think the coefficients of Starbucks and Dunkin Donuts remain significant after considering the county information? 
#       Write your opinion briefly by commenting (#).

# NO, the significance and p-value increased as the county added into the model as a cut off point of 0.05, 
# the two variables starbucks and dunkin_donuts have high pvalue and less significance. df





# Q6-2. Which county, on average, has the most expensive house prices? (note that all counties in this data are in Boston, MA)
#       Write your opinion briefly by commenting (#).

df_spread %>% group_by(county) %>% 
  summarise(average=mean(house_price_index)) %>% 
  arrange(desc(average)) %>%
  head(1)

# Norfolk has the most expensive house prices in average. 

#===================================================================



#======= Question 7 (1 Point) =======
# Q7. Based on your analysis, do you agree or disagree that Starbucks is the bellwether of rise in house prices?
#     Write your opinion briefly by commenting (#).


# NO, I am not agree with this proposal that the starbucks is the bellwether of rising the house's price
# as the code provided below starbucks is in rank of 7 in term of a low estimate coefficient and also a higher pvalue
# comparing the other variables. Therefore, I think that is not a good idea to predict the houses with the starbucks
# around them.
# It could be a good creteria if we would like to consider it arounf the malls and shoping centers or where the 
# businesses are, BUT not a houses. We can think a bout this that if we were bought a nice and expensive house
# but beside us was a starbucks and everyday tons of people and cars were on the line and busy trafic around house
# noises and other things gatherign people, etc which these could lead to decrease the price of the ouse who wanted 
# pay for a house to be comfortable in it and have a kalm. Therefore, it is not a good idea except might it 
# were located in the malls and businuss palaces, etc.
# Also, I have provided a code below tha were the starbucks stands comparing other variables.

#====================== Write R code HERE ==========================
library(broom)

tidy(modelLogPlusCounty) %>%
  select(estimate,p.value,term) %>%
  arrange(p.value,desc(estimate))


###### End of the code#####

#===================================================================
