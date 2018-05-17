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



###################################################################################################
########## 3장 확률분포 ########

#포아송 분포의 정규근사
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
