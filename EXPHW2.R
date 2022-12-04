prob0234 <- read.table("https://www.stat.uiowa.edu/~ernli/DOEdata/problem0234.txt",
                       header = TRUE)
prob0234
t.test(prob0234$Caliper1, prob0234$Caliper2, paired = TRUE)
diff = prob0234$Caliper1 - prob0234$Caliper2
qqnorm(diff)
qqline(diff)
t.test(prob0234$Caliper1, prob0234$Caliper2, var.equal = TRUE)
prob0226 <- read.table("https://www.stat.uiowa.edu/~ernli/DOEdata/problem0226.txt",
                       header = TRUE)
prob0226
x1 = mean(prob0226$Machine1)
x2 = mean(prob0226$Machine2)
xsd1 = sd(prob0226$Machine1)
xsd2 = sd(prob0226$Machine2)
nxt = 10
zstat = (x1 - x2) / sqrt((xsd1/nxt) + (xsd2/nxt))
zstat
pnorm(.134, mean = 0, sd = 1)
zSE = sqrt((xsd1/nxt) + (xsd2/nxt))
margin = x1 - x2
mult = zSE *1.96
margin - mult
margin + mult

zalpha2 = 1.96
zbeta = 0.84
sigma1 = 0.015
sigma2 = 0.018
deltachange = 0.01
n = (qnorm(1-alpha/2) + qnorm(1-beta) ) ^ 2 * (sigma1^2 + sigma2^2) / deltachange^ 2 
n
delta = -0.01
sigma1 = 0.015
sigma2 = 0.018
beta = 1- 0.8
alpha = 0.05

n = (qnorm(1-alpha/2) + qnorm(1-beta) ) ^ 2 * (sigma1^2+sigma2^2) / delta^2
n
zscore = qnorm(1-alpha/2)
Nmean = delta / sqrt(sigma1^2/n1 + sigma2^2/n2)
type2 = pnorm(zscore, mean = Nmean, sd=1) - pnorm(-zscore, mean = Nmean, sd = 1)
power = 1 - type2
power

prob0231 <- read.table("https://www.stat.uiowa.edu/~ernli/DOEdata/problem0231.txt",
                       header = TRUE)
prob0231
var.test(prob0231$Thick95, prob0231$Thick100, ratio = 1)
y1 = mean(prob0231$Thick95)
y2 = mean(prob0231$Thick100)
thetahat = (y1 - y2)
df = 14
sd1 = sd(prob0231$Thick95)
sd2 = sd(prob0231$Thick100)
Sp = sqrt( (7*sd1^2 + 7*sd2^2) / df)
SE = Sp * sqrt(1/10 + 1/10)
t.stat = thetahat / SE
t.stat

boxplot(prob0231)
var.test(prob0231$Thick95, prob0231$Thick100, ratio = 1)
qqnorm(prob0231[ , "Thick95"], main = "Normal QQ - Thick95")
qqline(prob0231[ , "Thick95"])
qqnorm(prob0231[ , "Thick100"], main = "Normal QQ - Thick100")
qqline(prob0231[ , "Thick100"])

delta = 2.5 
sigma = 1.884
n = 8
alpha = 0.05
df = n+n-2
tcritical = qt(1-alpha, df)
ncp = abs(delta) / sqrt(2*sigma^2/n)
beta = pt(tcritical, df, ncp)
power = 1-beta
power
power.t.test(n = 8, delta = 2.5, sd = 1.884, sig.level = 0.05,
             power = NULL,
             type = "two.sample", alternative = "one.sided", strict = FALSE)
tcritical
ncp = abs(delta) / sqrt(2*sigma^2/n)
ncp
power.t.test(n = 8, delta = 1.5, sd = 1.884, sig.level = 0.05,
             power = NULL,
             type = "two.sample", alternative = "one.sided", strict = FALSE)
power.t.test(n = 6, delta = 2.5, sd = 1.884, sig.level = 0.05,
             power = NULL,
             type = "two.sample", alternative = "one.sided", strict = FALSE)
power.t.test(n = 8, delta = 2.5, sd = 1.884, sig.level = 0.10,
             power = NULL,
             type = "two.sample", alternative = "one.sided", strict = FALSE)
power.t.test(n = 8, delta = 2.5, sd = 1.884, sig.level = 0.10,
             power = NULL,
             type = "two.sample", alternative = "one.sided", strict = FALSE)
delta = 2.5
n1 = 6
n2 = 10
alpha = 0.05
sigma = 1.884
df = n1 + n2 - 2 
tcritical = qt(1-alpha, df)
ncp = abs(delta) / sqrt(2*sigma^2 / (n1+n2))
beta = pt(tcritical, df, ncp)
power = 1 - beta
power

power.t.test(n = NULL, delta = 2.5, sd = 1.884, sig.level = 0.05,
             power = 0.85,
             type = "two.sample", alternative = "one.sided", strict = FALSE)
power.t.test(n = NULL, delta = 2.5, sd = 1.884, sig.level = 0.05,
             power = 0.9,
             type = "two.sample", alternative = "one.sided", strict = FALSE)
power.t.test(n = NULL, delta = 1.5, sd = 1.884, sig.level = 0.05,
             power = 0.85,
             type = "two.sample", alternative = "one.sided", strict = FALSE)
power.t.test(n = NULL, delta = 2.5, sd = 1.884, sig.level = 0.10,
             power = 0.85,
             type = "two.sample", alternative = "one.sided", strict = FALSE)
y1 = 9.366625
y2 = 6.846625
tstats = (y1 - y2) / (sigma * sqrt((1/n1) + (1/n2)))
tstats
tcritical

t.test(prob0231$Thick95, prob0231$Thick100, paired = TRUE)




