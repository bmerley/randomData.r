prob0515 <- read.table("https://www.stat.uiowa.edu/~ernli/DOEdata/problem0515.txt",
                       header = TRUE)
prob0515
with(prob0515, interaction.plot(Temperature, Glass, Light))
summary(aov(prob0515$Light~factor(prob0515$Temperature) * factor(prob0515$Glass), prob0515))
fitfull = lm(prob0515$Light ~ (prob0515$Temperature + I(prob0515$Temperature^2)) * (prob0515$Glass+ I(prob0515$Glass^2)), prob0515)
summary(fitfull)

ngrid <- 20
Temperature <- with(prob0515, seq(min(prob0515$Temperature), max(prob0515$Temperature), length = ngrid))
Glass <- with(prob0515, seq(min(prob0515$Glass), max(prob0515$Glass), length = ngrid))
grid <- expand.grid(Temperature = Temperature, Glass = Glass)
yhat <- predict(fitfull, grid)
yhat <- matrix(yhat, length(Temperature), length(Glass))
persp(Temperature, Glass, yhat, theta = -45, expand = 0.75, ticktype = "detailed")

prob0530 <- read.table("https://www.stat.uiowa.edu/~ernli/DOEdata/problem0530.txt",
                       header = TRUE)
prob0530
with(prob0530, interaction.plot(Doping, Temperature, Current))

summary(aov(prob0530$Current~((prob0530$Doping) + I(prob0530$Doping^2)) * ((prob0530$Temperature) + I(prob0530$Temperature^2)), prob0530))




prob0609 <- read.table("https://www.stat.uiowa.edu/~ernli/DOEdata/problem0609.txt",
                       header = TRUE)
prob0609
summary(aov(prob0609$Vibration~ prob0609$Bit * prob0609$Speed), prob0609)

with(prob0609, interaction.plot(prob0609$Bit, prob0609$Speed, prob0609$Vibration))
with(prob0609, interaction.plot(prob0609$Speed, prob0609$Bit, prob0609$Vibration))


summary(lm(prob0609$Vibration ~ prob0609$Bit * prob0609$Speed, prob0609))

ngrid <- 20
x1 <- with(prob0609, seq(min(prob0609$Bit), max(prob0609$Bit), length = ngrid))
x2 <- with(prob0609, seq(min(prob0609$Speed), max(prob0609$Speed), length = ngrid))
grid <- expand.grid(x1 = x1, x2 = x2)
yhat = predict(lm(prob0609$Vibration ~ x1 * x2, prob0609), grid)





