# 베르누이 분포
theta = 0.6
xx = c(0, 1)
barplot(dbinom(xx, size=1, prob=theta), names.arg=c("X=0", "X=1"), ylab="P(x)", main="pmf of Bernoulli distribution")

set.seed(0)
x <- rbinom(100, size=1, prob=theta)
x

barplot(table(x))


# 이항 분포
N <- 10
theta <- 0.6
xx = 0:N
barplot(dbinom(xx, size=N, prob=theta), 
        names.arg=0:N, ylab="P(x)", 
        main="pmf of binomial distribution")

set.seed(0)
x <- rbinom(100, size=N, prob=theta)
x

barplot(tabulate(x + 1, nbins=N+1), names.arg=0:N)

y1 <- dbinom(0:N, size=N, prob=theta) * 100
y2 <- tabulate(x + 1, nbins=N+1)
t <- rbind(y1, y2)
rownames(t) <- c("theoretic", "simulation")
colnames(t) <- 0:N
t

barplot(t, beside=TRUE, legend.text=TRUE, args.legend=list(x="topleft"))


# 정규분포 
mu <- 0
std <- 1
xx <- seq(-5, 5, length=100)
plot(xx, dnorm(xx, mean=mu, sd=std), type="l",
     xlab="x", ylab="p(x)", main="pdf of normal distribution")


set.seed(0)
x <- rnorm(100, mean=mu, sd=std)
x

hist(x, freq=F)
lines(density(x))
rug(x)

qqnorm(x)
qqline(x)


set.seed(0)
x <- runif(100)
qqnorm(x)
qqline(x)

# CLT 
xx <- seq(-2, 2, length=100)
N <- c(1, 2, 10)
layout(matrix(1:6, 3, 2, byrow=T))
for (i in 1:3) {
  X <- matrix(runif(1000 * N[[i]]) - 0.5, nrow=N[[i]])
  S <- colSums(X)/sqrt(N[[i]])
  hist(S)
  qqnorm(S)
}


# t-분포

library(quantmod)
symbols = c('^GDAXI', '^GSPC', 'YHOO', 'MSFT')
getSymbols(symbols)

df <- data.frame(merge(GDAXI[,4], GSPC[,4], YHOO[,4], MSFT[,4]))
colnames(df) <- c('GDAXI', 'GSPC', 'YHOO', 'MSFT')
df <- na.omit(df)
tail(df)

library(tidyr)
dm <- as.matrix(df)
df <- data.frame(t(t(dm)/dm[1,]))
df[["date"]] <- rownames(df)
dfm <- gather(df, stock, price, -date)

library(ggplot2)
ggplot(data=dfm, aes(x=date, y=price, group=stock, col=stock)) + geom_line() + 
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

dmr <- (dm[-1,]/dm[-dim(dm)[[1]],]-1) * 100
layout(matrix(1:4, nrow=2, byrow=T))
for (i in 1:4) {
  qqnorm(dmr[,i], main="")
  title(symbols[[i]])
}


library(RColorBrewer)
cols <- brewer.pal(n=5, name="Set1")
xx <- seq(-5, 5, length=200)
i <- 0
dfs <- c(1, 2, 5, 10, 20)
for (df in dfs) {
  plot(xx, dt(xx, df=df), type="l", xlab="", ylab="", main="",
       xlim=c(-5, 5), ylim=c(0, 0.4), col=cols[i])
  par(new=TRUE)
  i <- i + 1
}
legend("topleft", legend=paste("df = ", dfs), col=cols, lty=1)


# 카이 제곱 분포
library(RColorBrewer)
cols <- brewer.pal(n=5, name="Set1")
xx <- seq(0, 25, length=200)
i <- 1
dfs <- c(1, 2, 5, 10, 20)
for (df in dfs) {
  plot(xx, dchisq(xx, df=df), type="l", xlab="", ylab="", main="",
       xlim=c(0, 25), ylim=c(0, 0.4), col=cols[i])
  par(new=TRUE)
  i <- i + 1
}
legend("topleft", legend=paste("df = ", dfs), col=cols, lty=1)


# F-분포

library(RColorBrewer)
cols <- brewer.pal(n=5, name="Set1")
xx <- seq(0.01, 3, length=200)
plot(xx, df(xx, df1=1, df2=1), type="l", xlab="", ylab="", main="", xlim=c(0.01, 3), ylim=c(0, 2), col=cols[1]); par(new=TRUE)
plot(xx, df(xx, df1=2, df2=1), type="l", xlab="", ylab="", main="", xlim=c(0.01, 3), ylim=c(0, 2), col=cols[2]); par(new=TRUE)
plot(xx, df(xx, df1=5, df2=2), type="l", xlab="", ylab="", main="", xlim=c(0.01, 3), ylim=c(0, 2), col=cols[3]); par(new=TRUE)
plot(xx, df(xx, df1=10, df2=1), type="l", xlab="", ylab="", main="", xlim=c(0.01, 3), ylim=c(0, 2), col=cols[4]); par(new=TRUE)
plot(xx, df(xx, df1=20, df2=20), type="l", xlab="", ylab="", main="", xlim=c(0.01, 3), ylim=c(0, 2), col=cols[5]); par(new=TRUE)
legend("topleft", col=cols, lty=1,
       legend=c("F(1,1)", "F(2,1)", "F(5,2)", "F(10,1)", "F(20,20)"))


# 결합/주변 분포   
mu <- c(0, 0)
cov <- matrix(c(2, -1, -1, 18), nrow=2, byrow=T)

library("mnormt")
xx <- seq(-5, 5, length=100)
yy <- seq(-5, 5, length=100)
zz <- matrix(dmnorm(expand.grid(xx, yy), mu, cov), nrow=100)

contour(xx, yy, zz)
library("plot3D")
contour3D(xx, yy, zz, colvar=zz)

# 이산분포의 결합
pmf = rbind(c(0, 0, 0, 0, 1, 1),
            c(0, 0, 1, 2, 1, 0),
            c(0, 1, 3, 3, 1, 0),
            c(0, 1, 2, 1, 0, 0),
            c(1, 1, 0, 0, 0, 0))
pmf = pmf/sum(pmf)
pmf

image(t(apply(pmf, 2, rev)), col=gray(100:0/100), xaxt="n", yaxt="n")

# 주변분포
pmf_marginal_x <- colSums(pmf)
matrix(pmf_marginal_x, nrow=1)

pmf_marginal_y <- rowSums(pmf)
matrix(pmf_marginal_y, nrow=length(pmf_marginal_y))

par(mfrow=c(2,2))
layout(matrix(c(1, 3, 2, 0), nrow=2, byrow=T), c(2, 1), c(2, 1))
contour(xx, yy, zz)
plot(xx, rowSums(zz), type="l", xlab="", ylab="", ylim=c(0, 3))
plot(colSums(zz), yy, type="l", xlab="", ylab="", xlim=c(0, 1.5))

