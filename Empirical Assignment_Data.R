library(haven)
library(clubSandwich)
mydata <- read_dta("/Users/kacperszostakow/Desktop/data.dta")
attach(mydata)

#create variables
mydata$dt <- dltt + dlc
attr(mydata$dt, 'label') <- 'Debt - Total'
mydata$leverage <- mydata$dt/at
attr(mydata$leverage, 'label') <- 'Leverage - Variable'
mydata$tangibility <- ppent/at
attr(mydata$tangibility, 'label') <- 'Tangibility - Variable'
mydata$mkvalt <- prcc_f * csho
attr(mydata$mkvalt, 'label') <- 'Market Value'
mydata$bvoe <- at - mydata$dt
attr(mydata$bvoe, 'label') <- 'Book Value of Equity'
mydata$market_to_book <- (at - ceq + mydata$mkvalt)/at
attr(mydata$market_to_book, 'label') <- 'Market-to-Book'
mydata$logsale <- log(sale) # In log(sale) : NaNs produced. Replace with 0
mydata$logsale[!is.finite(mydata$logsale)] <- 0
mydata$logsale[is.nan(mydata$logsale)] <- 0
attr(mydata$logsale, 'label') <- 'Log of Sale'
mydata$profitability <- ebitda/at
attr(mydata$profitability, 'label') <- 'Profitability'
detach(mydata)

#apply filters
library(dplyr)
mydataclean <- mydata[mydata$fic == "USA" & mydata$ipodate >= "1993-01-01" & mydata$at > 0,]
attach(mydataclean)

#### a) 
length(unique(gvkey)) # we have 1354 obs & 677 firms
summary(mydataclean) # find MIN, MAX and AVERAGE
sapply(mydataclean %>% select_if(is.numeric), sd, na.rm = TRUE) # find standard deviations

#### c)
m <- lm(leverage ~ tangibility) # run regression of leverage on tangibility
summary(m)

#Manual t test for beta tangibility = 1, used 1349 DF
betatan <- coef(m)[[2]]
sdtan <- summary(m)$coefficients[2,2]
t <- (betatan - 1)/sdtan
t #tsatistic
p1 <- pt(t,1349)
p1  #p-value
#With linearHypotheis function, reports F statistic, have to install package "car
library(car) 
linearHypothesis(m,"tangibility=1")

#### d)
n <- lm(leverage ~ tangibility + market_to_book + logsale + profitability) # add other variables to the model
summary(n)

#Manual t test for beta tangiblity = -2*beta profitability
nvcov <- vcov(n)
nvcov
betatan2 <- coef(n)[[2]]
betaprof <- coef(n)[[5]]
sdtp <- sqrt(2.486722e-03 + 4 * 2.363806e-04 + 4 * -9.986766e-06 )
t2 <- (betatan2 + 2 * betaprof)/sdtp
t2 #t statistic
p2 <- (1-pt(t2,671)) #I used 671 degrees of freedom because regression says 668 for 5 coefficients 
p2 #p-value
# using linearHypothesis function, but gives F statistics, package car
linearHypothesis(n,"tangibility=-2*profitability")

#### e)
levels(factor(fyear)) # first level is 2005
levels(factor(substr(sic, 1, 2)))
d <- lm(leverage ~ tangibility 
        + market_to_book 
        + logsale 
        + profitability 
        + factor(fyear) 
        + factor(substr(sic, 1, 2)))
summary(d) # we get too many independent variables that are not significant

# we should group year into after 2009 and other. We should also group SIC codes for relevance

mydataclean$dummyYear <- as.numeric(fyear > 2009) # variable has value 1 for years > 2009 and 0 for <= 2009
temp1 <- levels(factor(substr(sic, 1, 2)))
temp1 <- temp1[-c(2, 3, 4, 5, 6, 40, 41, 42, 43, 44, 45, 46)]
mydataclean$dummySic <- as.numeric(substr(sic, 1, 2) %in% temp1) # this gives us 0 for firms that belong to finance, insurance, real estate, construction and mining industries

detach(mydataclean) # this is to update DataFrame that is attached
attach(mydataclean)

d2 <- lm(leverage ~ tangibility 
         + market_to_book 
         + logsale 
         + profitability 
         + dummyYear 
         + dummySic)
summary(d2) # now we have more significant results

#### f)
coef_test(d, vcov = "CR1", 
          cluster = gvkey, test = "naive-t")[1:2,]

coef_test(d, vcov = "CR2", 
          cluster = gvkey, test = "Satterthwaite")[1:2,] # test for d from e)

coef_test(d2, vcov = "CR1", 
          cluster = gvkey, test = "naive-t")[1:2,]

coef_test(d2, vcov = "CR2", 
          cluster = gvkey, test = "Satterthwaite")[1:2,] # test for d2 from e)

#---------------------------------------------

#### g)   -   we will first take d from e)
library(sandwich)
library(lmtest)
library(plm)
# estimate a combined time and entity fixed effects regression model via lm()
g <- plm(leverage ~ tangibility 
         + market_to_book 
         + logsale 
         + profitability 
         + factor(fyear) 
         + factor(substr(sic, 1, 2)), data = mydataclean, index = "gvkey", model = "within")
summary(g)

coef_test(g, vcov = "CR1", 
          cluster = gvkey, test = "naive-t")[1,]

coef_test(g, vcov = "CR2", 
          cluster = gvkey, test = "Satterthwaite")[1,]

# obtain a summary based on clusterd standard errors (adjustment for autocorrelation + heteroskedasticity)
coeftest(g, vcov = vcovHC, type = "HC1")

g2 <- plm(leverage ~ tangibility 
          + market_to_book 
          + logsale 
          + profitability 
          + dummyYear 
          + dummySic, data = mydataclean, index = "gvkey", model = "within")
summary(g2)

coef_test(g2, vcov = "CR1", 
          cluster = gvkey, test = "naive-t")[1,]

coef_test(g2, vcov = "CR2", 
          cluster = gvkey, test = "Satterthwaite")[1,]

# obtain a summary based on clusterd standard errors (adjustment for autocorrelation + heteroskedasticity)
coeftest(g2, vcov = vcovHC, type = "HC1")

#### h)
h <- plm(leverage ~ tangibility 
         + market_to_book 
         + logsale 
         + profitability 
         + factor(fyear), data = mydataclean, index = "gvkey", model = "within")
summary(h)

coef_test(h, vcov = "CR1", 
          cluster = gvkey, test = "naive-t")[1,]

coef_test(h, vcov = "CR2", 
          cluster = gvkey, test = "Satterthwaite")[1,]

# obtain a summary based on clusterd standard errors (adjustment for autocorrelation + heteroskedasticity)
coeftest(h, vcov = vcovHC, type = "HC1")

h2 <- plm(leverage ~ tangibility 
          + market_to_book 
          + logsale 
          + profitability 
          + dummyYear, data = mydataclean, index = "gvkey", model = "within")
summary(h2)

coef_test(h2, vcov = "CR1", 
          cluster = gvkey, test = "naive-t")[1,]

coef_test(h2, vcov = "CR2", 
          cluster = gvkey, test = "Satterthwaite")[1,]

# obtain a summary based on clusterd standard errors (adjustment for autocorrelation + heteroskedasticity)
coeftest(h2, vcov = vcovHC, type = "HC1")
