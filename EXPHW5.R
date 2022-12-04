prob0428 <- read.table("https://www.stat.uiowa.edu/~ernli/DOEdata/problem0428.txt",
                       header = TRUE)
prob0428
summary(aov(prob0428$Time ~ factor(prob0428$Order) + factor(prob0428$Operator) + factor(prob0428$Method), prob0428))
summary(aov(prob0428$Time ~ factor(prob0428$Operator) + factor(prob0428$Method), prob0428))
summary(aov(prob0428$Time ~ factor(prob0428$Order)  + factor(prob0428$Method), prob0428))
summary(aov(prob0428$Time ~  factor(prob0428$Method), prob0428))

plot(aov(prob0428$Time ~ factor(prob0428$Order) + factor(prob0428$Operator) + prob0428$Method, data = prob0428))
timeLm = lm(prob0428$Time~factor(prob0428$Order) + factor(prob0428$Operator) + prob0428$Method, data = prob0428)
nonadd = predict(timeLm)^2
anova(update(timeLm, .~. + nonadd))

crit.val = qt(.975, 6)
SE.diff = sqrt(1.75 * (1/4 + 1/4))
LSD = crit.val * SE.diff
c(crit.val = crit.val, SE.diff = SE.diff, LSD = LSD)

crit.val = sqrt(.5) * qtukey(.95, 4, 6)
SE.diff = sqrt(1.75 * (1/4+1/4))
HSD = crit.val * SE.diff
c(crit.val = crit.val, SE.diff = SE.diff, HSD = HSD)

6.167 - 1.75
4.417 / 4
(17.167   - 1.75) / 4 
library(lme4)
rate.lmer = lmer(prob0428$Time ~ (1|prob0428$Order) + (1|prob0428$Operator) + prob0428$Method, data = prob0428)
summary(rate.lmer)
library(emmeans)
rate.lm = lm(prob0428$Time ~ factor(prob0428$Order) + factor(prob0428$Operator) + prob0428$Method, data = prob0428)
emmeans(rate.lm, ~ Method)

prob0428 <- read.table("https://www.stat.uiowa.edu/~ernli/DOEdata/problem0428.txt",
                       header = TRUE)
prob0428missing <- prob0428
prob0428missing["1","Time"] <- NA
prob0428missing["14","Time"] <- NA

summary(aov(prob0428$Time ~ factor(prob0428$Order) + factor(prob0428$Operator) + factor(prob0428$Method), prob0428missing))
((6.167 + 17.167) + (4-1)*(1.75)) / (4-1) 
(6.167 + (4-1)*1.75) / 4 
(17.167 + (4-1)*1.75) / 4
((6 + 1) / (6 + 3)) * ((12+3) / (12+1)) * (9.528 / 1.75) 
((6+1) / (6+3)) * ((9 + 3) / (9+1)) * (2.85 / 1.75) 
((6+1) / (6+3)) * ((9 + 3) / (9+1)) * (5.604 / 1.75)

prob0441 <- read.table("https://www.stat.uiowa.edu/~ernli/DOEdata/problem0441.txt",
                       header = TRUE)
prob0441

summary(aov(prob0441$Time ~ factor(prob0441$Order) + factor(prob0441$Operator) + prob0441$Method + prob0441$Workplace , prob0441))

prob0427 <- read.table("https://www.stat.uiowa.edu/~ernli/DOEdata/problem0427.txt",
                       header = TRUE)
prob0427
summary(aov(prob0427$Time ~ factor(prob0427$Batch) + factor(prob0427$Day) + factor(prob0427$Catalyst), prob0427))

((3.86 + 3.06) + ((5-1)*3.13)) / 6 
((12 + 1) / (12 + 3)) * ((20+3) / (20+1)) * (3.24 / 3.13) 

