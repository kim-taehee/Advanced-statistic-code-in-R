### 1.이항 분포 ####
# 베르누이 분포
# 이항을 한번만 실행하면 베르누이 분포
theta = 0.6
xx = c(0, 1)
barplot(dbinom(xx, size=1, prob=theta), names.arg=c("X=0", "X=1"), ylab="P(x)", main="pmf of Bernoulli distribution")

set.seed(0)
x <- rbinom(100, size=1, prob=theta)
x

barplot(table(x))


# 이항 분포
# 두가지 확률을 가지는경우 독립적으로 n번 반복할때의 분포
# 이항 분포가 클경우 정규분포로 근사시킬 수 있다.
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

# 음이항 분보
# 이항분포에서 r번째 성공이전에 있었던 실패의 밀도함수
# NB(10,0.2)의 분포
x<-seq(0:100)
plot(x,dnbinom(x,10,0.2),type='h',xlab="y",ylab="f(y|r=10,theta=0.2) ") # 0.2의 성공확률에, 10번 성공했음


# 다항분포
# 다항분포는 이항분포의 확장으로 서로배반인 확률들이 2개 이상인 경우의 분포
install.packages("multinomRob")
library(multinomRob)
n=10; theta=c(0.5,0.2,0.3)
nofSample=1000
rMultinom=matrix(0,nofSample,3)
for( i in 1:nofSample){ rMultinom[i,]=rmultinomial(n,theta)}
hist(rMultinom[,1],prob=T,xlab = 'x1'); hist(rMultinom[,2],prob=T,xlab='x2')


# 포아송 분포
# 특정 시간동안 발생한 사건들의 수에 대한 분포
par(mfrow=c(1,2))
x<-c(0:12)
plot(x,dpois(x,2),xlab='x',ylap=(ylap="poisson, theta"))
lines(x,dnorm(x,2,sqrt(2)),lty=1)
x<-c(0:100)
plot(x,dpois(x,20),xlab='x',ylap=(ylap="poisson, theta"))
lines(x,dnorm(x,20,sqrt(20)),lty=1)
rx<-rpois(3000,2)
mean.x<-mean(rx)
par(mfrow=c(1,1))

##############################################

### 2.연속 분포 ####

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

# 감마 분포
# 감마 분포 : a번째 사건이 일어날 떄 까지 걸리는 시간에 대한 연속확률 분포
# 양의 상수 a(형태모수),b(척도모수에 대해 x의 밀도함수가  주어질 때 , 감마분포를 따른다.
# 지수분포는 감마분포의 특수한 경우로, a=1 인경우 지수분포라고 한다 = 포아송분포에서 다음 사건이 일어나기 까지의 시간 분포
# 또한 자유도 v를 갖는 카이제곱분포는 감마분포 (v/2,1/2)과 동일하다.  
par(mfrow=c(1,1))
x=seq(0,12,length=100)
plot(x,dgamma(x,1,1),type='l') # 지수분포
lines(x,dgamma(x,2,1),lty=2,col="green")
lines(x,dgamma(x,5,1),lty=3,col="red")
lines(x,dgamma(x,10,1),lty=4,col="blue")
legend(3,0.6,c("alpha=1,beta=1(지수분포)","alpha=2,beta=1",
               "alpha=5,beta=1","alpha=10,beta=1"),
                lty=(1:4),col=c("black","green","red","blue"))

# 카이 제곱 분포
library(RColorBrewer)
par(mfrow=c(1,1))
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


# t-분포

library(quantmod)
symbols = c('^GDAXI', '^GSPC', 'MSFT')
getSymbols(symbols)

df <- data.frame(merge(GDAXI[,4], GSPC[,4], MSFT[,4]))
colnames(df) <- c('GDAXI', 'GSPC', 'MSFT')
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
layout(matrix(1:3, nrow=2, byrow=T))
par(mfrow=c(2,2))
for (i in 1:3) {
  qqnorm(dmr[,i], main="")
  title(symbols[[i]])
}
par(mfrow=c(1,1))

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

# 베타 분포
# 디히클레 분포에서 k=2 인 분포이다.


# 디히클레 분포

# 디히클레 분포는 베타 분포의 확장.
# 사전분포를 사후분포가 같아지는 분포를 계산의 편리를 위해서 베이지안에 사용한다,
# 이러한 사전분포를 '켤레사전분포' (Conjugate prior distribution) 라 부르는데,
# 베타분포와, 디히클레 분포가 된다.

# 우리가 설정한 사전분포인 베타분포와 데이터 발생 분포인 이항분포의 조합으로 만들어지는 사후분포는 
# 베타분포의 모수가 사전분포의 α에 성공횟수를 더하고, β는 실패횟수를 더하여 업데이트 된 베타분포가 된다.
# 초기 설정된 사전 분포가 사후 분포에 영향을 많이 주기 떄문에, 
# n(시행횟수)가 적은 경우는 영향을 많이 미치고, 많은 경우는 MPE(MLE의 베이지안 버전)에 따라 
# frequancy 방법에 가까워져서 일반적인 추론통계에 가까워 진다.

# -- 이변량, 다변량 베이지안 통계의 차이점 
# 우도     | 켤레사전분포  | 사후확률분포
#------------------------------------------
# 이항분포 | 베타분포      | 베타분포
# 다항분포 | 디히클래 분포 | 디히클레 분포
# -----------------------------------------

rDirichlet<-function(n,k,theta){
  rDir<-matrix(0,n,k)
  for(nsim in 1:n){
    x <- c(1:k+1)
    for(i in 1:k+1) x[i]<-rgamma(1,theta[i])
    s<-sum(x)
    for(i in 1:k) x[i]<-x[i]/s
    rDir[nsim,1:k] <- x[1:k]
  }
  return(rDir)
}
nsim=5000
theta=c(3,4,2)
rDi=rDirichlet(nsim,2,theta)
plot(rDi[,1],rDi[,2],xlab = "x1",ylab="x2")





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


########################################################################
##### 3. 결합분포 #####
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

