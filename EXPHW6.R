prob0509 <- read.table("https://www.stat.uiowa.edu/~ernli/DOEdata/problem0509.txt",
                       header = TRUE)
prob0509
summary(aov(prob0509$Surface ~ factor(prob0509$Depth) * factor(prob0509$Feed), prob0509))

with(prob0509, interaction.plot(Depth, Feed, Surface, type = "b"))
qt.val = qtukey(.95, 4, 24)
SE.diff = sqrt(28.7 * ((1/4) + (1/4)))
HSD = qt.val / sqrt(2) * SE.diff
c(qt.val = qt.val, SE.diff = SE.diff, HSD=HSD)

TukeyHSD(aov(prob0509$Surface ~ factor(prob0509$Depth) * factor(prob0509$Feed), prob0509))
TukeyHSD(aov(prob0509$Surface ~ factor(prob0509$Feed) * factor(prob0509$Depth), prob0509))
tapply(prob0509$Surface, factor(prob0509$Depth), mean)
ybars = c(84.77778,  89.77778,  97.88889, 104.88889)
a = 4
b = 3
n = 1000
D = 8
MSE = 28.7
df.num = a-1
df.den = a*b*(n-1)
alpha = 0.05
( ncp = a * n * D^2 / (2*MSE) )
( Fcritical = qf(alpha, df.num, df.den, lower.tail = FALSE) )
( power = 1 - pf(Fcritical, df.num< df.den, ncp) )


prob0511 <- read.table("https://www.stat.uiowa.edu/~ernli/DOEdata/problem0511.txt",
                       header = TRUE)
prob0511
summary(aov(prob0511$Current ~ factor(prob0511$Phosphorous) * factor(prob0511$Glass), prob0511))

with(prob0511, interaction.plot(Phosphorous, Glass, Current, type = "b"))

summary(aov(prob0511$Current ~ factor(prob0511$Phosphorous) + factor(prob0511$Glass), prob0511))
TukeyHSD(aov(prob0511$Current~factor(prob0511$Phosphorous) + factor(prob0511$Glass), data = prob0511))


prob0523 <- read.table("https://www.stat.uiowa.edu/~ernli/DOEdata/problem0523.txt",
                       header = TRUE)
prob0523

prob0523m <- within(prob0523,
                    {H <- factor(prob0523$Hardwood);
                    C <- factor(prob0523$Cooking);
                    P <- factor(prob0523$Pressure)})
summary(aov(prob0523$Strength~H * C * P, prob0523m))
plot(aov(prob0523$Strength~factor(prob0523$Hardwood)+factor(prob0523$Cooking)+factor(prob0523$Pressure), prob0523))

with(prob0523, interaction.plot(Hardwood, Pressure, Strength))

prob0521 <- read.table("https://www.stat.uiowa.edu/~ernli/DOEdata/problem0521.txt",
                       header = TRUE)
prob0521
summary(aov(prob0521$Strength~factor(prob0521$Pressure) + factor(prob0521$Temperature), prob0521))

fitStrength = aov(prob0521$Strength~ factor(prob0521$Pressure) + factor(prob0521$Temperature), prob0521)
nonadd = predict(fitStrength)^2
anova(update(fitStrength, .~. + nonadd))

TukeyHSD(aov(prob0521$Strength~factor(prob0521$Pressure) + factor(prob0521$Temperature), data = prob0521))

with(prob0521, interaction.plot(Pressure, Temperature, Strength, type = "b"))
par(mfrow=c(2,2))
plot(aov(prob0521$Strength~factor(prob0521$Pressure) + factor(prob0521$Temperature), prob0521))

