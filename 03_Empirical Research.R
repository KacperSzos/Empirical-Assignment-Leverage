require(wooldridge)
attach(ceosal2)

m <- lm(salary ~ sales + ceoten)


summary(m)

lsalary <- log(salary)
m <- lm(lsalary ~ sales + ceoten)
summary(m)


lsales <- log(sales)
m <- lm(salary ~ lsales + ceoten)
summary(m)


m <- lm(lsalary ~ lsales + ceoten)
summary(m)


ceotensq <- ceoten^2
m <- lm(lsalary ~ ceoten + ceotensq)
summary(m)

ceoten_sales <- ceoten*sales
m <- lm(lsalary ~ sales + ceoten + ceoten_sales)
summary(m)

plot(ceoten, lsalary)
curve( coef(m)[1] + coef(m)[2]*x + coef(m)[3]*x^2, col="red", add=TRUE)

require(car)
lht(m,"ceoten = 0")
