a = 3 
n = 5
alpha = 0.01
ybars = c(95.2, 79.4, 100.4)
MSE = 15.6
( ncp = n * sum( ( ybars - mean(ybars)) ^ 2) / MSE )
( Fcritical = qf( alpha, (a-1) , (a*n-a), lower.tail = FALSE) )
( beta = pf(Fcritical, (a-1), (a*n-a), ncp))
( power = 1 - beta)

power.anova.test(groups=a, n=n, between.var = var(ybars), within.var = MSE,
                 sig.level = alpha, power = NULL)
power = 0.90
power.anova.test(groups = a, n=NULL, between.var = var(ybars), within.var = MSE,
                  sig.level = alpha, power = 0.90)
prob0409 <- read.table("https://www.stat.uiowa.edu/~ernli/DOEdata/problem0409.txt",
                       header = TRUE)
prob0409

summary(lm(prob0409$Growth ~ factor(prob0409$Solution) + factor(prob0409$Days), data = prob0409))

summary(aov(prob0409$Growth ~ factor(prob0409$Days) + factor(prob0409$Solution), data = prob0409))
growthLM = lm(prob0409$Growth~ factor(prob0409$Days) + factor(prob0409$Solution), data = prob0409)
nonadd = predict(growthLM)^2
anova(update(growthLM, .~. + nonadd))
interaction.plot(factor(prob0409$Days), factor(prob0409$Solution), prob0409$Growth)
par(mfrow=c(2,2))
plot(aov(prob0409$Growth~factor(prob0409$Days+factor(prob0409$Solution), 
                                data = prob0409)))
summary(aov(prob0409$Growth ~ factor(prob0409$Days), data = prob0409))
TukeyHSD(aov(prob0409$Growth ~ factor(prob0409$Days) + factor(prob0409$Solution), data = prob0409),
          "factor(prob0409$Solution)")
summary(lm(prob0409$Growth ~ factor(prob0409$Solution) + factor(prob0409$Days), data = prob0409))
TukeyHSD(aov(prob0409$Growth ~ factor(prob0409$Solution) + factor(prob0409$Days), data = prob0409),
         "factor(prob0409$Solution)")

a = 3
b = 4
D = 10
MSE = 8.6
alpha = 0.05
( ncp = b*(D^2) / (2*MSE) )
Fcritical = qf( alpha, (a-1), (a-1)*(b-1), lower.tail = FALSE)
beta = pf( Fcritical, (a-1), (a-1)*(b-1), ncp)
power = 1 - beta
power
prob0409Copy <- prob0409
prob0409Copy["9", "Growth"] <- NA
prob0409Copy

summary(aov(prob0409Copy$Growth ~ factor(prob0409Copy$Days) + factor(prob0409Copy$Solution), 
            data = prob0409Copy))

prob0412 <- read.table("https://www.stat.uiowa.edu/~ernli/DOEdata/problem0412.txt",
                       header = TRUE)
prob0412
summary(aov(prob0412$Hardness ~ factor(prob0412$Tip) + factor(prob0412$Coupon), data = prob0412))
plot(aov(prob0412$Hardness  ~ factor(prob0412$Tip) + factor(prob0412$Coupon), data = prob0412))
prob0412LM = lm(prob0412$Hardness ~ factor(prob0412$Tip) + factor(prob0412$Coupon), data = prob0412)
nonadd = predict(prob0412LM) ^ 2
anova(update(prob0412LM, .~. + nonadd))
interaction.plot(factor(prob0412$Tip), factor(prob0412$Coupon), prob0412$Hardness)
par(FALSE)

factoC = as.factor(prob0412$Coupon)
contrasts(factoC)
factorT = as.factor(prob0412$Tip)
contrasts(factorT)
summary( lm(prob0412$Hardness ~ factorT + factoC) )
library(multcomp)
facTip = as.factor(prob0412$Tip)
myHSD = glht(aov(prob0412$Hardness ~ factor(prob0412$Coupon) + facTip, data = prob0412),
             linfct = mcp(facTip="Tukey"))
summary(myHSD)
prob0412Copy <- prob0412
prob0412Copy["16", "Hardness"] <- NA
summary(aov(prob0412Copy$Hardness ~ factor(prob0412Copy$Tip) + factor(prob0412Copy$Coupon), 
            data = prob0412Copy))






