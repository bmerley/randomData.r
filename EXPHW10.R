p0815 <- read.table("https://www.stat.uiowa.edu/~ernli/DOEdata/problem0815.txt",
                    header = TRUE)
p0815
p0815$A <- ifelse(p0815$Time == 2.5, -1, 1)
p0815$B <- ifelse(p0815$Concentration == 14, -1, 1)
p0815$C <- ifelse(p0815$Pressure == 60, -1, 1)
p0815$D <- ifelse(p0815$Temperature == 225, -1, 1)
p0815
summary(lm(p0815$Yield~ p0815$A * p0815$B * p0815$C * p0815$D, p0815))
alias(lm(p0815$Yield~p0815$A * p0815$B * p0815$C * p0815$D, p0815))
library(gplots)
qqnorm(aov(p0815$Yield~p0815$A * p0815$B * p0815$C * p0815$D, p0815), label = TRUE)

p0852 <- read.table("https://www.stat.uiowa.edu/~ernli/DOEdata/problem0852.txt",
                    header = TRUE)
p0852
alias(lm(p0852$Gain~p0852$A*p0852$B*p0852$C*p0852$D*p0852$E*p0852$F, p0852))
summary(lm(p0852$Gain~p0852$A*p0852$B*p0852$C*p0852$D*p0852$E*p0852$F, p0852))
qqnorm(aov(p0852$Gain~p0852$A*p0852$B*p0852$C*p0852$D*p0852$E*p0852$F, p0852), label = TRUE)

p1420 <- read.table("https://www.stat.uiowa.edu/~ernli/DOEdata/problem1420.txt",
                    header = TRUE)
p1420
summary(aov(p1420$Reflectance~p1420$))


