problem0701
summary(aov(problem0701$Life~problem0701$Block + problem0701$Speed * problem0701$Geometry * problem0701$Angle, problem0701))
summary(aov(problem0701$Life~problem0701$Speed * problem0701$Geometry * problem0701$Angle, problem0701))
prob0704

summary(lm(prob0704$Life~prob0704$Block + prob0704$Speed * prob0704$Geometry * prob0704$Angle, prob0704))
c(tapply(prob0704$Life, prob0704$Speed, mean),
eff = diff(tapply(prob0704$Life, prob0704$Speed, mean)))
c(tapply(prob0704$Life, prob0704$Speed*prob0704$Geometry, mean),
eff = diff(tapply(prob0704$Life, prob0704$Speed*prob0704$Geometry, mean)))

library(gplots)
qqnorm(aov(prob0704$Life~prob0704$Block + prob0704$Speed*prob0704$Geometry*prob0704$Angle,prob0704), label = TRUE)
summary(aov(prob0704$Life~prob0704$Block+prob0704$Geometry+prob0704$Angle+prob0704$Speed:prob0704$Angle + prob0704$Geometry:prob0704$Angle, prob0704))
prob0713r
summary(lm(prob0713r$UEC~prob0713r$Block + prob0713r$Laser*prob0713r$Pulse*prob0713r$Cell*prob0713r$Writing, prob0713r))
qqnorm(aov(prob0713r$UEC~prob0713r$Block + prob0713r$Laser*prob0713r$Pulse*prob0713r$Cell*prob0713r$Writing, prob0713r), label = TRUE)

summary(aov(prob0713r$UEC~prob0713r$Laser + prob0713r$Cell + prob0713r$Writing + prob0713r$Laser:prob0713r$Cell + prob0713r$Cell:prob0713r$Writing, prob0713r))


prob0724

coded <- function(x) ifelse(x == "-", -1, 1)
lifeRep1 <- prob0724[prob0724$Rep == 1,]
A <- coded(lifeRep1$Speed)
B <- coded(lifeRep1$Geometry)
C <- coded(lifeRep1$Angle)
lifeRep1$Block <- ifelse(A*B*C < 0, 1, 2)
lifeRep1 <- lifeRep1[order(lifeRep1$Block),]

lifeRep2 <- prob0724[prob0724$Rep == 2,]
A <- coded(lifeRep2$Speed)
B <- coded(lifeRep2$Geometry)

lifeRep2$Block <- ifelse(A*B > 0, 1, 2)
lifeRep2 <- lifeRep2[order(lifeRep2$Block),]

lifeRep3 <- prob0724[prob0724$Rep == 3,]

B <- coded(lifeRep3$Geometry)
C <- coded(lifeRep3$Angle)
lifeRep3$Block <- ifelse(A*B*C < 0, 1, 2)
lifeRep3 <- lifeRep3[order(lifeRep3$Block),]
partialConfounding <- rbind(lifeRep1, lifeRep2, lifeRep3)
partialConfounding

summary(aov(partialConfounding$Life ~ factor(partialConfounding$Block):factor(partialConfounding$Rep) + partialConfounding$Speed * partialConfounding$Geometry * partialConfounding$Angle, partialConfounding))
summary(lm(partialConfounding$Life ~ partialConfounding$Block + partialConfounding$Speed * partialConfounding$Geometry * partialConfounding$Angle, partialConfounding))
library(car)
vif(lm(partialConfounding$Life ~ partialConfounding$Block + partialConfounding$Speed * partialConfounding$Geometry * partialConfounding$Angle, partialConfounding))
