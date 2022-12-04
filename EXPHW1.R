randomvalues = runif(12)
randomvalues
sort(randomvalues)
names = c("A","B","C","D","E","F","G","H","I","J","K","L")
names
RNs = runif(12)
RNs
names[order(RNs)]
sample(names)
my.plan = data.frame(treatment = rep(c("trt1","trt2", "trt3"), 4), subj = sample(names))
my.plan
my.plan = my.plan[sample(1:12), ]
my.plan = cbind(run=1:12, my.plan)
my.plan
prob0228 <- read.table("https://www.stat.uiowa.edu/~ernli/DOEdata/problem0228.txt",
                       header = TRUE)
prob0228
var.test(prob0228$Type1, prob0228$Type2, ratio = 1)
t.test(prob0228$Type1, prob0228$Type2, var.equal = TRUE)
qqnorm(prob0228[ ,"Type1"], main = "Normal QQ - Type1");
qqline(prob0228[ ,"Type1"]);
qqnorm(prob0228[ , "Type2"], main = "Normal QQ - Type2");
qqline(prob0228[ , "Type2"]);

t.test(prob0228$Type1, prob0228$Type2, var.equal = FALSE)
boxplot(prob0228)


