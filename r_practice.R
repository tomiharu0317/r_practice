df <- read.csv("SalesData.csv", row.names=1)
barplot(df$April, names.arg=rownames(df), las=1, main="Sales in April", cex.main=3)
options(scipen=100)
par(mar=c(5.1,6.1,5.1,2.1))


pie(df$April)
df2 <- df[order(df$April, decreasing=TRUE),]
pie(df2$April, clockwise=TRUE, labels=rownames(df2), main="Proportion of sales in April", cex.main=3)

df <- read.csv("HeightWeightData.csv")
par(mar=c(7,6,4,2))
plot(df$height, df$weight)

df <- read.csv("MathScores.csv")
x <- df$Math.test.scores
hist(x, col="orange", main="Histogram of the math scores")
hist(x, breaks="scott") 
hist(x, breaks=5)
hist(x, breaks=c(50,60,70,80,90,100))
hist(x, freq=FALSE)

h <- hist(x, freq=FALSE)
h

df <- read.csv("mini-exam3-2.csv")
machine1 <- subset(df, df$Machine==1)
machine2 <- subset(df, df$Machine==2)
machine1_weight <- machine1$Weight
machine2_weight <- machine2$Weight
par(mar=c(7,6,4,2))
hist(machine1_weight, main="Histogram of Machine1 weight")
hist(machine2_weight, main="Histogram of Machine2 weight")



df <- read.csv("RentData.csv")
df[1:5, ]

df <- read.csv("practice3-4-1.csv")
table(df$Answer)



df <- read.csv("practice3-4-2.csv")
df
cor(df$Export, df$Import)

#標本分散
varp <- function(x){var(x)*(length(x)-1)/length(x)}

#標本標準偏差
sdp <- function(x) {
  varp <- var(x)*(length(x)-1)/length(x)
  return(sqrt(varp))
}


# Rによる母平均の検定と推定
df <- read.csv("mini-exam4-1.csv")
df
df$days
x <- c(df$days)
x
t_0 <- (mean(x)-12.0)/sqrt(var(x)/length(x))
t_0
qt(p=0.01, df=49)
mean(x)+qt(0.005, 9)*sqrt(var(x)/50)
mean(x)+qt(0.995, 9)*sqrt(var(x)/50)

# Rによる母分散の検定と推定
df <- read.csv("mini-exam4-2.csv")
df
x <- c(df$income)
x
s <- sum((x-mean(x))^2)
chi_0 <- s/(4.0)^2
chi_0
qchisq(p=0.05, df=99)
s/qchisq(0.975, 99)
s/qchisq(0.025, 99)

# Rによる一元配置分散分析
df <- read.csv("mini-exam5-1.csv")
level_sum <- colSums(df)
level_mean <- colMeans(df)
T <- sum(df)
total_mean <- mean(colMeans(df))
r <- nrow(df)
n <- r*ncol(df)
CT <- T^2/n
S_T <- sum(df^2)-CT
S_A <- sum(level_sum^2)/r-CT
S_E <- S_T - S_A
phi_A <- ncol(df) - 1
phi_E <- n - ncol(df)
V_A <- S_A / phi_A
V_E <- S_E / phi_E
F_0 <- V_A / V_E
F_0
# 棄却域
qf(p=0.95, df1=phi_A, df2=phi_E)

# Rによる繰り返しのある二元配置分散分析
df <- read.csv("mini-exam5-2.csv")
df
options(digits=10)
summary(aov(Yield ~ Fertilizer * Crop, data = df))

# Rによる繰り返しのない二元配置分散分析
df <- read.csv("mini-exam5-3.csv")
df
summary(aov(removed~brand+temperature, data = df))
summary(aov(Distance~Golfer+Brand, data = df))