p0624 <- read.table("http://www.stat.uiowa.edu/~ernli/DOEdata/problem0624.txt",
                    header = TRUE)
p0624$C <- ifelse(p0624$Carbonation == 10, -1, 1)
p0624

summary(aov(p0624$Deviation~p0624$Carbonation*p0624$Pressure*p0624$Speed, p0624))

lm(formula = Deviation ~ p0624$Carbonation * p0624$Pressure * p0624$Speed, data = p0624)

library(gplots)
qqnorm(aov(p0624$Deviation~ p0624$Carbonation * p0624$Pressure * p0624$Speed, p0624) , label = TRUE)

summary(aov(p0624$Deviation~p0624$Carbonation * p0624$Pressure, p0624))
coded <- function(x) ifelse(x == "-", -1, 1)
summary(lm(p0624$Deviation~ coded(p0624$Carbonation) * coded(p0624$Pressure), p0624))
summary(aov(p0624$Deviation~p0624$Carbonation * p0624$Pressure + factor(p0624$Carbonation):factor(p0624$Pressure):factor(p0624$Speed),
            p0624))




prob0626 <- read.table("http://www.stat.uiowa.edu/~ernli/DOEdata/problem0626.txt",
                    header = TRUE)
prob0626

UEC.fit <- (lm(prob0626$UEC~ prob0626$Laser * prob0626$Pulse * prob0626$Cell * prob0626$Writing, prob0626))
summary(aov(UEC.fit))
( effs = 2*coef(UEC.fit) [-1])

library(gplots)
qqnorm(aov(UEC.fit), label = TRUE)
PSE = function(e) {
  abseff = abs(e)
  s0 = 1.5 * median(abseff)
  1.5 * median(abseff[abseff < 2.5*s0])
}
PSE(effs)
cbind(effs, pseudo.t = effs/PSE(effs))

summary(aov(prob0626$UEC~prob0626$Laser*prob0626$Cell*prob0626$Writing, prob0626))
summary(lm(prob0626$UEC~ prob0626$Laser + prob0626$Cell + prob0626$Writing + prob0626$Laser*prob0626$Cell + prob0626$Laser*prob0626$Cell, prob0626))

prob0627 <- read.table("http://www.stat.uiowa.edu/~ernli/DOEdata/problem0627.txt",
                       header = TRUE)
prob0627
summary(lm(prob0627$UEC~ prob0627$Laser * prob0627$Pulse * prob0627$Cell * prob0627$Writing
           + I(prob0627$Laser^2) + I(prob0627$Pulse^2) + I(prob0627$Cell^2) + I(prob0627$Writing^2), prob0627))

UEC.fit2 <- (lm(prob0627$UEC~ prob0627$Laser * prob0627$Pulse * prob0627$Cell * prob0627$Writing
                + I(prob0627$Laser^2) + I(prob0627$Pulse^2) + I(prob0627$Cell^2) + I(prob0627$Writing^2), prob0627))
qqnorm(aov(UEC.fit2), label = TRUE)
PSE = function(e) {
  abseff = abs(e)
  s0 = 1.5 * median(abseff)
  1.5 * median(abseff[abseff < 2.5*s0])
}
PSE(effs)
cbind(effs, pseudo.t = effs/PSE(effs))

summary(aov(prob0627$UEC~ prob0627$Laser * prob0627$Pulse * prob0627$Cell * prob0627$Writing
           + I(prob0627$Laser^2) + I(prob0627$Cell^2) + I(prob0627$Writing^2), prob0627))


summary(aov(prob0627$UEC~ prob0627$Laser * prob0627$Cell + prob0627$Cell * prob0627$Writing
            + I(prob0627$Laser^2) + I(prob0627$Cell^2) + I(prob0627$Writing^2) 
            + factor(prob0627$Laser):factor(prob0627$Pulse):factor(prob0627$Cell):factor(prob0627$Writing), prob0627))



