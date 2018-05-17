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
    post<-0.95*prior/(0.95*prior+0.7*(1-prior))
  }
  else
    post<-0.05*prior/(0.05*prior+0.3*(1-prior))
}

prior<-c(1:13)
post<-c(1:12)
data<-c(1,0,1,1,1,1,1,1,1,0,1,0)
prob<-matrix(0,12,2)
prior[1]<-0.9
for(i in 1:12){
  post[i]<-post.prob(data[i],prior[i])
  prob[i,]<-c(prior[i],post[i])
  prior[i+1]<-post[i]
}
prob

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