# p.27
aa<-function(pf) {50*pf/(41*pf+9)}
x<-c(0:10)*0.1
x
prob<-aa(x)
plot(x,x,type = "l",xlab = "P(F)",ylab = "prob")
lines(x,prob,lty=2)

# p.31
post.prob<-function(s,prior){
  if(s==1){
    post<-0.95*prior/(0.95*prior+0.7*(1-prior)) # 사후확률1 
    # 기계가 이상 없을 때, 부품이 정상일 경우 0.95 , 기계가 이상할 떄 정상일 경우 0.7
  }
  else
    post<-0.05*prior/(0.05*prior+0.3*(1-prior)) #사후확률2 
    # 기계가 이상 없을 때, 부품이 비정상일 경우, 기계가 이상할때 비정상일 경우 0.3
} # 사전 사후 확률을 계속 곱해주는 함수 

prior<-c(1:13)
post<-c(1:12)
data<-c(1,0,1,1,1,1,1,1,1,0,1,0)
prob<-matrix(0,12,2)
prior[1]<-0.9
for(i in 1:12){
  post[i]<-post.prob(data[i],prior[i])
  prob[i,]<-c(prior[i],post[i])
  prior[i+1]<-post[i]
} #posterior 의 길이 만큼 반복해 줌 
prob

# 연습문제1 : 어느 도시에 거주하는 주민들 중 약 2%가 색맹이라고 하자. 
#  만약 색맹을 검사가 이때 옳게 판정할 확률(true positive), 아닌데 옳게 판정할 확률 0.001(false positive,1종오류)
#  이때의 수식은?
postProbEye<-function(e,prior){
  if(e==0.02){
    post<-0.98*prior/(0.98*prior+0.001*(1-prior)) # 사후확률1 
    # 무작위 주민이 색맹인 경우 0.02*0.98 , false positive - 0.98*0.001
  }
  else
    post<-0.02*prior/(0.02*prior+0.999*(1-prior)) #사후확률2 
  # 무작이 주민이 색맹인데 검사x(2종오류), 아닌데 검사x(false negative) 
} # 사전 사후 확률을 계속 곱해주는 함수 

prior<-c(1:2)
post<-c(1:1)
data<-c(0.02,0.02)
prob<-matrix(0,2,2)
prior[1]<-0.02
for(i in 1:2){
  post[i]<-post.prob(data[i],prior[i])
  prob[i,]<-c(prior[i],post[i])
  prior[i+1]<-post[i]
} #posterior 의 길이 만큼 반복해 줌 
prob # 1,2번째 다 양성일 경우
# 1번 환자는 사전확률 0.02 사후확률 0.03으로 사후확률이 1.5배 크다(odds ratio)
# 2번 환자는 사전확률 0.03 사후확률 0.0005로 앞사람에 비해 오진가능성이 커서 사후확률이 적다

# 연슴문제2 ,p.32
# P(A|D) = P(D|A)*P(A) /[ P(D|A)*P(A) + P(D|!A=B)*P(!A=B) ] 
Pad = (4/10)*(2/3) / ( (4/10)*(2/3) + (7/10)*(1/3)); Pad
#  P(B|D) = P(D|B)*P(B) /[ P(D|A)*P(A) + P(D|B*P(B) ] 
Pbd = (7/10)*(1/3) / ( (4/10)*(2/3) + (7/10)*(1/3)); Pbd
Pad+Pbd

########## 3장 확률분포 ########

library("ggplot2")
par(mflow=c(1,2))
x<-c(0:12)
plot(x,dpois(x,2),xlab = "x",ylab = "p(x|theta=2)")
lines(x,dnorm(x,2,sqrt(2)),lty=1)
x<-c(0:100)
plot(x,dpois(x,20),xlab = "x",ylab = "p(x|theta=20")
lines(x,dnorm(x,20,sqrt(20)),lty=1)

#베르누이 시행의 우도함수 10번 중 p가 3일 때
theta=seq(0,1,length = 200)
ltheta=120*theta^3*(1-theta)^7
plot(theta,ltheta,type = "l")
abline(v=0.3,lty=2)

#이때 x고정일때 최대를 만들어주는 theta가 MLE

#########################################################
# 고전적 통계추론의 문제점
# 고전적 추론은 가상적인 반복시험을 반복하기 때문에 실제와는 다르다

theta=seq(0.1,0.9,0.1)
x=5;n=20
joint=theta**x*(1-theta)**(n-x)
s.joint=sum(joint)
post=joint/s.joint
post

# 이항분포에서의 베이지안 추론
# theta ~beta(a,b)
a=1; b=1
# X ~ B(n,theta)
n=40; x=15

theta = seq(0,1,length.out = 50)
prior.theta = dbeta(theta,a,b) #prior
likhd.theta = dbinom(x,n,theta)
joint.xtheta = prior.theta * likhd.theta
post.theta = dbeta(theta,a+x,b+n-x) # 음수가 되면 안그려짐 ,beta(16,26)

# 시각화
par(mfrow=c(2,2))
plot(theta,prior.theta,type = "l")
abline(v=(a-1)/(a-1+b-1),lty=2)
mtext("prior: p(theta)",side=3)

plot(theta,likhd.theta,type = "l")
abline(v=x/n,lty=2)
mtext("likelihood: p(theta)",side = 3)

plot(theta,joint.xtheta,type = "l")
abline(v=(a+x-1)/(a+b+n-2),lty=2)
mtext("prior x likelihood: p(theta) x p(x|theta)",side = 3)


plot(theta,post.theta,type = "l")
abline(v=(a+x-1)/(a+b+n-2),lty=2)
mtext("posterior: p(theta|x)",side = 3)

########## 4장 통계추론 ########

# 우도함수와 우도비
#  - 성공확률이 Θ인 베르누이 시행을 10번 반복할때 Θ의 우도함수는 L(Θ|x)  x가 3일때
theta <-seq(0,1,length=200)
ltheta<-(10*9*8/3*2*1)*theta^3*(1-theta)^7
plot(theta,ltheta,type="l")
abline(v=0.3,lty=2)

# 우도가 클수록 관측치 x로 미루어볼때 Θ의 참 값일 가능성이 큰데, 이를  Maximun Likelyhood Estimation 라고 한다. 
# Θ에 대한 추정치로 2개의  값을 비교할 때  우도함수의 비율을  종종 사용하며  이를 우도비 라고 한다.
# 우도비를 사용할때 충분통계량을 알면 유용한데, 모수 Θr가  벡터일 때  가장 차원이  작은 통계랑을 찾는 것이 중요한데 
#  이를 최소 충분통계량이라 한다.

#  -- 기본적인 베이지안 추론의 절차 ---
#    관심모수에 대한 이론,경험 등으로 사전분포를 정한다(잘 정해야함)
#    관측변수 X를 정하고 실험등을 통하여 관측값 x와 x의 조건부확률을 얻는다.
#    사전분포(prior)와 조건부확률을 통해 사후분포(posterior)를 구하고 추론에 사용한다.

#  베이지안 구간추정
#    주어진 신뢰구간에서 Θ 에 대한  사후밀도 값들이  높은 것들을 모으는 것이다.
#     ex) N(Θ,4)  분포로 부터 16개의 표본을  추출한 결과 표본평균은 0.3이었다. 
#          Θ에 대한 사전 분포로 1을  가정하고  사후분포로  유도하면 Θ의 사후분포는 N(0.3,1/4)     이때의  95%  베이지안  신뢰구간
theta<-seq(-3,3,length=500)
plot(theta,dnorm(theta,0.3,0.5),ldy=";")
abline(v=qnorm(c(0.049,0.999),0.3,0.5),lty=2)
abline(v=qnorm(c(0.025,0.975),0.3,0.5),lty=1)  # 더 좋은 신뢰구간 

# 하지만 복잡하기 때문에, 사후분위수를 찾아 대략적인 최대사후구간으로 사용한다.





#### 5. 이항분포에 대한 베이지안 추론#####

# 새로운 교육방법에 학생에게 영향을 줄 확률 세타.
# 이항 몬테카를로 방법 추론 (세타의 사후분포 추정)
theta <- rbeta(2000,x+a,n-x+b) #  ~ Beta(a,b)
hist(theta,prob=TRUE,main="histogram of theta")
lines(density(theta))
mean_theta<-mean(theta)
abline(v=mean_theta,lty=2)
# 상하위 2.5%씩 
quantile(theta,c(.025,.975))  # simulation quantiles
qbeta(c(.025,.975),x+a,n-x+b) # theoretical quantiles  , ~ Beta(a,b)

#simulation-based estimates
mean(theta); var(theta)
#theoretical estimates
(a+x)/(a+b+n); (a+x)*(b+n-x)/( (a+b+n+1)*(a+b+n)^2 )

# odds ratio inference
a=b=1
x=15;n=40
theta=rbeta(10000,a+x,b+n-x)
eta=log(theta/(1-theta))
hist(eta,prob=TRUE,main="histogram of eta")
lines(density(eta),lty=2)
mean_eta=mean(eta)
var_eta=var(eta)


# Beta(2,10)을 사전 분포로, Beta(17,35), Beta(22,40)을 비교해보자
theta <- seq(0,1,length=50)
a<-2; b<-10; x<-15;n<-40; z<-5; m<-10
prior_theta = dbeta(theta,a,b)
post_theta = dbeta(theta,x+a,n-x+b)
post2_theta = dbeta(theta,x+a+z,n-x+b+m-z)
plot(theta,post_theta,type = "l",col="blue")
lines(theta,post2_theta,lty=2,col="green")
lines(theta,prior_theta,lty=3,col="red")
legend(.5,3,legend=c(paste("beta(",a,"",b,")prior"),
                          paste("beta(",a+x,"",b+n-x,")posterior" ),
                          paste("beta(",a+x+z,"",b+n-x+m-z,")posterior")),      
                          lty=c(3,1,2)      ,col=c("red","blue","green")
        )
                          

# 베이지안 신뢰구간
#   세타의 사전분포로 균일 분포를 가정하고 이항관측치로 n=10,x=2 가 얻어졌을때 세타의
#   베이지안 신뢰구간을 구해보자. 이 경우 최대사후구가을 수리적으로 구하기는 어려우므로
#   누적확률 2.5%와 97.5%에 해당하는 구간을 대략적인 최대사후구간이라 하자
a=1;b=1
n=10; x=2
theta=seq(0,1,0.01)
plot(theta,dbeta(theta,a+x,n-x+b),type="l",xlim=c(-0.1,1),ylab="p(theta|x)")
bayes_CI=qbeta(c(0.025,.975),a+x,b+n-x) # beta 분포의 quantile
abline(v=qbeta(c(0.025,0.975),a+x,n-x+b))
p=x/n # uniformed distribution
freq_CI=c(p-1.96*sqrt(p*(1-p)/n),p+1.96*sqrt(p*(1-p)/n) ) 
abline(v=c(p-1.96*sqrt(p*(1-p)/n),p+1.96*sqrt(p*(1-p)/n) ),lty=2 )
legend(.55,1.5,legend = c(paste("Bayesian interval"),
                          paste("classical interval")),
       lty=c(1,2),bty="n")

