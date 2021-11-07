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

# Rによる単回帰分析
df <- read.csv("practice6-1.csv")
df
lm(df$Brth15to17~df$PovPct)
anova(lm(Brth15to17~PovPct, data=df))
summary(lm(Brth15to17~PovPct, data=df))

df <- read.csv("mini-exam6-1.csv")
df
lm(df$Accuracy~df$Distance)
summary(lm(df$Accuracy~df$Distance))


# Rでの分岐処理と繰り返し処理
df <- read.csv("practice6-1.csv")
df
# 結果を格納結果を格納
result <- lm(df$Brth15to17~df$PovPct)
names(result$coefficients)
beta_0 <- result$coefficients[1]
beta_1 <- result$coefficients[2]
# 推定出生率の計算関数
birthRate <- function(PovPct) {
  rate <- beta_0 + (beta_1 * PovPct)
  return(rate)
}
# for を使った計算
Brth <- df$Brth15to17
PovPct <- df$PovPct
count <- 0
n <- length(Brth)

for (i in 1:n) {
  povpct <- PovPct[i]
  rate_actual <- Brth[i]
  rate_estimated <- birthRate(povpct)
  if (rate_actual > rate_estimated) {
    count <- count + 1
  }
}

print(count)

# for を使わないで計算する
estimated_rate <- c(PovPct*beta_1 + beta_0)
sum(Brth > estimated_rate)

# Rによるシミュレーション実験
n_exp <- 10000 #実験回数
n_trial <- 1000　#コイン投げの回数
count <- 0
for (i in 1:n_exp) {
  unif_01 <- runif(n_trial, 0, 1)
  coin_flip <- unif_01 < 0.5
  if (sum(coin_flip) > 550) {
    count <- count + 1
  }
}

count / n_exp

# Rによるシミュレーション実験(2)
n_exp <- 10000
n_trial <- 1000
mean <- 0
sd <- 1
count <- 0
for (i in 1:n_exp) {
  sample <- rnorm(n_trial, mean, sd)
  x_hat <- mean(sample)
  mu_lower <- x_hat - 1.96 * (sqrt(sd / n_trial))
  mu_upper <- x_hat + 1.96 * (sqrt(sd / n_trial))
  if (mu_lower < mean && mean < mu_upper) {
    count <- count + 1
  }
}

count / n_exp












