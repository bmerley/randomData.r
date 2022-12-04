prob0311 <- read.table("https://www.stat.uiowa.edu/~ernli/DOEdata/problem0311.txt",
                       header = TRUE)
prob0311
summary(aov(prob0311$Strength~factor(prob0311$Technique), data = prob0311))

boxplot(prob0311$Strength ~ prob0311$Technique, prob0311,
        ylab = expression(paste("Rate (", ring(lb), "/in^2)")),
        xlab = "Technique (w)")
plot(prob0311$Strength~prob0311$Technique, prob0311,
     ylab = expression(paste("Rate (", ring(lb), "/in^2)")),
     xlab = "Technique (w)")
a = 4    
n = 4
DFerr = a*(n-1)
MSerr = 12826
( tcritical = qt(.025, DFerr, lower.tail = FALSE) )
( SE = sqrt(2*MSerr/n) )
( LSD = tcritical * SE)
( dt1 = )


library(agricolae)
prob0311LM <- lm(prob0311$Strength ~ factor(prob0311$Technique), data = prob0311)
( LSD.test(prob0311LM, "factor(Technique)") )
library(emmeans)
emmeans(prob0311LM, ~ "Technique")
pairs(emmeans(prob0311LM, ~ Technique), adjust = "none")
qqnorm(residuals(prob0311LM), datax = TRUE, pch = 12)
plot(fitted(prob0311LM), residuals(prob0311LM), pch = 12)

bartlett.test(prob0311$Strength ~ factor(prob0311$Technique), data = prob0311)

( dt1vsdt3 = 2971 - 2934 )
( CI.dt1vsdt3 = dt1vsdt3 + c(-1, 1) * tcritical * SE)
( ttest = dt1vsdt3/ SE)
( pval = 2 * pt(abs(ttest), DFerr, lower.tail = FALSE))

( q.critval = qtukey(.05, a, DFerr, lower.tail = FALSE) )
( SE = sqrt(2 * MSerr / n))
( HSD = q.critval/sqrt(2) * SE )
TukeyHSD(aov(prob0311$Strength ~ factor(prob0311$Technique), data = prob0311))

pairwise.t.test(prob0311$Strength, prob0311$Technique, p.adjust.method = "bonferroni")
pairs(emmeans(prob0311LM, ~ Technique), adjust = "Bonferroni")
library(agricolae)
( LSD.test(prob0311LM, "factor(Technique)", p.adj = "bonferroni") )

prob0330 <- read.table("https://www.stat.uiowa.edu/~ernli/DOEdata/problem0330.txt",
                       header = TRUE)
prob0330
summary(aov(prob0330$Life ~ factor(prob0330$Brand), data = prob0330))
prob0330LM <- lm(prob0330$Life ~ factor(prob0330$Brand), data = prob0330)
plot(residuals(prob0330LM), main="plot(residuals(prob0330LM))", pch = 15)
library(emmeans)
emmeans(prob0330LM, ~ Brand)
(95.2 + 100.4) / 2 
97.8 - 79.4
(79.4 + 100.4) / 2
95.2 - 89.9


a=3
n=5
DFerr = a*(n-1)
alpha = .05
MSerr = 15.6
(tcritical = qt(.025, DFerr, lower.tail = FALSE) )
(SE = sqrt(2 * MSerr/n) ) 
( LSD = tcritical * SE)


( q.critval = qtukey(alpha, a, DFerr, lower.tail=FALSE) ) 
( HSD = q.critval/sqrt(2) * SE)


( F.critval = qf(.05, a-1, DFerr, lower.tail = FALSE) )
( MSD.Scheffe = sqrt((a-1) * F.critval) * sqrt(2*MSerr/n) )
library(DunnettTests)
cvSDDT(a-1, alpha = 0.05, alternative = "B", DFerr)
( d.critval = 2.502)
( SE = sqrt(2*MSerr/n))
( Diff.Dunnett = d.critval* SE)


