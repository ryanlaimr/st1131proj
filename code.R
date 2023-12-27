#### R code for statistical report
### Student ID: A0287898U
### Name: Lai Zheyuan

rm(list = ls()); setwd('~/NUS/23fall/ST1131/Data')
housing_data = read.csv('house_selling_prices_OR.csv')
names(housing_data); head(housing_data)
attach(housing_data)

### PART I - Exploring the variables

## summarize the variables
summary(housing_data)

## House size and house price
plot(House.Size, HP.in.thousands) # positive relation between house size and house price
cor(House.Size, HP.in.thousands) # correlation is 0.71
# Can consider the House.Size as a regressor

## Acres and house price
plot(Acres, HP.in.thousands) # no apparent relationship between acres and house price
cor(Acres, HP.in.thousands) # 0.23
# Cannot consider Acres as a regressor

## Lot size and house price
plot(Lot.Size, HP.in.thousands) # no apparent relationship between lot size and house price
cor(Lot.Size, HP.in.thousands) # 0.23
# Cannot consider Lot.Size as a regressor

## Bedrooms and house price
boxplot(HP.in.thousands~Bedrooms)
# Bedrooms may be a regressor

## Bathrooms and house price
boxplot(HP.in.thousands~T.Bath)
# T.Bath may be a regressor

## Age and house price
plot(Age, HP.in.thousands) # no apparent relationship between age and house price
cor(Age, HP.in.thousands) # -0.17
# Age cannot be a regressor

## Garage and house price
boxplot(HP.in.thousands~Garage) # mean nearly the same
# Garage cannot be a regressor

## Condition and house price
boxplot(HP.in.thousands~Condition) # mean nearly the same
# Condition cannot be a regressor

## Age category and house price
boxplot(HP.in.thousands~Age.Category) # mean differs
# Age.Category can be a regressor

# In conclusion, House.Size and Age.Category can be a regressor,
# Bedrooms and T.Bath may be a regressor

### PART II - Building Model

## Build model M1, consider House.Size and Age.Category as regressors
M1 = lm(HP.in.thousands~House.Size + Age.Category, data = housing_data); summary(M1)
# Result of t-test
# p_value of Age.CategoryN = 00287 => not very significant
# p_value of Age.CategoryO = 0.22 => not significant
# p_value of Age.CategoryVO = 0.199 => not significant
# So cannot include Age.Category as a regressor

# Check the outliers and influential points
SR = rstandard(M1); which(SR > 3 | SR < -3) # 70, 98, 127, 130
Cook = cooks.distance(M1); which(Cook > 1) # No influential points

# Check residual plots
plot(M1$fitted.values, SR); abline(h = 0, col = 'red')
hist(SR, prob = TRUE) # not normal
qqnorm(SR); qqline(SR, col = 'red') # not normal
# need to change model => try to exclude Age.Category

## Build model M2, consider House.Size as a regressor
M2 = lm(HP.in.thousands~House.Size, data = housing_data); summary(M2)
# Result of t-test: both intercept and House.Size are significant

# Check the plot of M2
plot(House.Size, HP.in.thousands); abline(M2, col = 'red')
# funnel shape => cannot conclude constant variance

# Check residual plot
SR = rstandard(M2)
hist(SR, prob = TRUE) # Some observations lie outside of (-3, 3)
qqnorm(SR); qqline(SR, col = "red") # both longer tails => not normal

# SR vs Predicted y^
plot(M2$fitted.values, SR, xlab = 'Predicted Response'); abline(h=0, col = "red")
# Some points with SR lying outside of (-3, 3)
# funnel shape => not constant variance

# Check the outliers and influential points
which(SR > 3 | SR < -3) # 70, 98, 127, 130
Cook = cooks.distance(M1); which(Cook > 1) # 127
# try to remove the influential point (index = 127)
housing_data = housing_data[-127,]

## Build a new model M3, remove the influential point (index = 127)
M3 = lm(HP.in.thousands~House.Size, data = housing_data); summary(M3)

# Check the residual plots
SR = rstandard(M3); hist(SR, prob = TRUE)
qqnorm(SR); qqline(SR, col = 'red')
plot(M3$fitted.values, SR, xlab = 'Predicted Values'); abline(h=0, col = 'red')
# Results are similar to M2, need to add explanatory variables

## Build model M4, try to add Bedrooms and T.Bath as regressors
M4 = lm(HP.in.thousands~House.Size + Bedrooms + T.Bath, data = housing_data); summary(M4)
# Result of t-test: p-value of Bedrooms = 0.16 => not significant => remove the regressor

## Build model M5, use regressors House.Size and T.Bath
M5 = lm(HP.in.thousands~House.Size + T.Bath, data = housing_data); summary(M5)
# Result of t-test: both regressors are significant, but intercept is not significant

# Check the outliers and influential points
SR = rstandard(M5); which(SR > 3 | SR < -3) # 70, 98, 171
Cook = cooks.distance(M5); which(Cook > 1) # no influential points

# Check residual plots
hist(SR, probability = TRUE) # Normal despite outliers
qqnorm(SR); qqline(SR, col = 'red') # Nearly normal
plot(M5$fitted.values, SR); abline(h = 0, col = 'red') # Not funnel shape => constant variance

# Model M5 is the final model
# HP(in thousand)^ = 19.96 + 0.06 * House.Size + 46.66 * T.Bath