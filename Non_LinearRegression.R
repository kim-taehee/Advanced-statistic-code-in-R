#######################################################
# Non-liner Regression Analysis
# EXtended Regression Analysis
#######################################################
# 비선형 최소자승법

# 1. Cobb-Douglas production Function  콥 더글라스 생산함수 p.164 ####
# 자본량과 노동량을 투입하여 생산량을 측정한다.
# non liner function : y=Ak^a*L^b  (A:기술계수,K:자본스톡, L:노동)
# 생산함수에서 a+b >1 수확체증, a+b=1 수확일정, a+b<1 수확체감 
xk=c(10,15,20,30,40,50,55,65,70,80,90,95)
xl=c( 5,10,12,15,20,30,35,40,45,50,55,60)
yy=c(15,22,25,28,33,35,40,48,50,53,55,57)
nn=nls(yy~a*xk^b*xl^c,start = list(a=1,b=0.5,c=0.5))
summary(nn)
(1-pchisq(1.98,9))

# 변수변환 선형모형
ll=lm(log(yy)~log(xk)+log(xl))
summary(ll)

# 비선형 모형과 선형모형의 추정값 비교
fitted(nn)
exp(fitted(ll)) # 자연로그 취햇으니 다시 자연로그 지수화
e1=yy-fitted(nn) # 비선형모형 잔차
e2=yy-exp(fitted(ll)) # 선형모형 잔차
(sum(e1^2)) # 제곱합
(sum(e2^2)) # 제곱합

# 2. SSasymp 초기값 무지정 점근적 비선형 회귀분석 사례 p. 166 ####
# 투입 요소량에 따른 산출요소량의 비선형 함수관계
# y=Asym + (R0 - Asym) * exp(-exp(lrc)*X) : 수정 지수곡선으로 알려짐
xx=seq(1,15)
yy=c(100,85,73,61,50, 42,36,32,29,27, 26,25,24,23,23)
nn=nls(yy~SSasymp(xx,Asym,respo,lrc))
summary(nn)

# ssasymp plot
plot(c(1,15),c(20,100),type = "n",
     main = "Non-liner regression plot \n Self-Start Asympote",
     xlab = "xx: 투입요소량",ylab="yy:산출요소량"
     )
cols=c(4,2)
pchs=c(17,16)
points(xx,yy,pch=pchs[1],col=cols[1])  # 측정값
points(xx,predict(nn)[1:15],pch=pchs[2],col=cols[2])  #추정값
dd=seq(1,15,0.2)
# kk=rep(1:1,15) 선과는 관계가 없음
p1=predict(nn,data.frame(xx=dd))
legend("topright",legend = c("실험측정값","모형추정값"), col=cols,pch=pchs)
lines(dd,p1) # ??? dd없이 그리면 다른 선인데? 
abline(h=coef(nn)[1],lty=2)  # estimate line
aa=signif(coef(nn)[1],3)   # estimate value
text(5,20,paste("Asymptotic Line: yy=",aa))

# 3. SSasymp Off 초기값 무지정 점근적 offset 비선형 회귀분석 사례 p.169 ####
xx=c(1,2,4,5,6, 8,9,10,13,15, 18,20,24,28,30)
yy=c(2,15,29,34,45, 55,63,70,76,80, 83,85,86,87,87)
nn=nls(yy~SSasympOff(xx,Asym,lrc,C0))
summary(nn)
# plot
plot(c(0,30),c(0,95),type = "n",
     main = "Non-linear regression Plot \n
     Self-start Asymptote-Offset", xlab="xx:투입요소량", ylab="yy:산출요소량")
cols=c(4,2)
pchs=c(17,16)
points(xx,yy,pch=pchs[1],col=cols[1])  # 측정값
points(xx,predict(nn)[1:15],pch=pchs[2],col=cols[2])  #추정값
dd=seq(1,30,0.2)
p1=predict(nn,data.frame(xx=dd))
legend("bottomright",legend = c("실험측정값","모형추정값"), col=cols,pch=pchs)
lines(dd,p1) # ??? dd없이 그리면 다른 선인데? 
abline(h=coef(nn)[1],lty=2)  # estimate line
aa=signif(coef(nn)[1],3)   # estimate value
c=signif(coef(nn)[3],3)   # c0 value
text(8,88,paste("Asymptotic Line: yy=",aa))
text(8,85,paste("Offset Values: C0=",cc))

# 4. SSasympOrig  초기값 무지정 점근적 원점회귀 비선형 회귀분석 사례 p.171 ####
# 투입량과 산출량의 비선형 함수관계에서, x=0 일때 y=0 이다.(초기값이 0이지 뭐)
xx=c(1,2,4,5,6, 8,9,10,13,15, 18,20,24,28,30)
yy=c(2,15,29,34,45, 55,63,70,76,80, 83,85,86,87,87)
nn=nls(yy~SSasympOrig(xx,Asym,lrc))
summary(nn)
# plot
plot(c(0,30),c(0,95),type = "n",
     main = "Non-linear regression Plot \n
     Self-start Asymptote Origin", xlab="xx:투입요소량", ylab="yy:산출요소량")
cols=c(4,2)
pchs=c(17,16)
points(xx,yy,pch=pchs[1],col=cols[1])  # 측정값
points(xx,predict(nn)[1:15],pch=pchs[2],col=cols[2])  #추정값
dd=seq(0,30,0.2)
p1=predict(nn,data.frame(xx=dd))
legend("bottomright",legend = c("실험측정값","모형추정값"), col=cols,pch=pchs)
lines(dd,p1) # ??? dd없이 그리면 다른 선인데? 
abline(h=coef(nn)[1],lty=2)  # estimate line
aa=signif(coef(nn)[1],3)   # estimate value
text(8,91,paste("Asymptotic Line: yy=",aa))
text(8,87,paste("Offset Values: C0=",0))

# 5. SSbiexp 초기값 무지정 이중지수 비선형 회귀분석 사례 p.173 ####
# 투입량과 산출량의 비선형 함수 관계에서, 이중지수(Bi-Exponential) 비선형 함수를 파악
xx=c(1,2,4,5,6,8,9,10,13,15,18,20,24,28,30)
yy=c(1,20,31,34,43,60,66,75,79,82,84,85,86,86,87)
nn=nls(yy~SSbiexp(xx,A1,lrc1,A2,lrc2)) #여기선 지정안했지만 현업에 A1이나 lrc1 값이 있겟지
summary(nn)
#단조증가에서 단조감소로 바뀌는 함수
# plot , xx가 커질 수록 0에 수렴한다 
plot(c(0,80),c(0,95),type = "n",
     main = "Non-linear regression Plot \n
     Self-start Asymptote Origin", xlab="xx:투입요소량", ylab="yy:산출요소량")
cols=c(4,2)
pchs=c(17,16)
points(xx,yy,pch=pchs[1],col=cols[1])  # 측정값
points(xx,predict(nn)[1:15],pch=pchs[2],col=cols[2])  #추정값
dd=seq(0,80,0.2)
p1=predict(nn,data.frame(xx=dd))
lines(dd,p1) 
legend("bottomright",legend = c("실험측정값","모형추정값"), col=cols,pch=pchs)

# 6. SSfol 초기값 무지정 1차 분할 비선형 회귀분석 사례 p.175 #### 
# 약품의 초기투여량에 대하여 시간에 따른 잔류량
# H1 : 시간에 따른 잔류량은 1차 분할 비선형관계이다.
# y=Dose*exp(lke + lka - lcl)*(exp(-exp(lke*input))-exp(-exp(lka*input))/(exp(lka)-exp(lke)))
tt=c(0,1,2,3,4, 6,8,10,12,15, 20,25,30,35,40)
yy=c(0,36,50,61,70, 64,58,53,48,39, 30,20,10,5,1)
nn=nls(yy~SSfol(Dose=10.0,tt,lke,lka,lcl))
summary(nn)
# plot
plot(c(0,50),c(0,95),type = "n",
     main = "Non-linear Regression Plot \n
     Self-Start 1st-Order Compartment", xlab="tt: 시간", ylab="yy:잔류량")
cols=c(4,2)
pchs=c(17,16)
points(tt,yy,pch=pchs[1],col=cols[1])  # 측정값
points(tt,predict(nn)[1:15],pch=pchs[2],col=cols[2])  #추정값
dd=seq(0,50,0.2)
p1=predict(nn,data.frame(tt=dd))
lines(dd,p1) 
legend("topright",legend = c("실험측정값","모형추정값"), col=cols,pch=pchs)

# 7. SSfpl,SSlogis 초기값 무지정 로지스틱 비선형 회귀분석 p.177 ####
# y=Asym/(1+exp((xmid-input)/scal))
tt=c(0,seq(2,28,2))
yy=c(10,12,15,19,27, 40,55,65,73,78, 82,84,86,87,87)
nn=nls(yy~SSfpl(tt,A,B,xmid,scal))
summary(nn)
# plot
plot(c(0,30),c(0,90),type = "n",
     main = "Non-linear Regression Plot \n
     Self-Start Logistic Growth", xlab="tt: 시간", ylab="yy:개체수")
cols=c(4,2)
pchs=c(17,16)
grid()
points(tt,yy,pch=pchs[1],col=cols[1])  # 측정값
points(tt,predict(nn)[1:15],pch=pchs[2],col=cols[2])  #추정값
dd=seq(0,30,0.2)
p1=predict(nn,data.frame(tt=dd))
lines(dd,p1) 
abline(h=coef(nn)[1],lty=2)
abline(h=coef(nn)[2],lty=2)
aa=signif(coef(nn)[1],3)
bb=signif(coef(nn)[2],3)
text(8,3,paste("Asymptotic Line: yy=",aa))
text(8,82,paste("Asymptotic Line: yy=",bb))
legend("bottomright",legend = c("실험측정값","모형추정값"), col=cols,pch=pchs)

# 8. SSgompertz 초기값 무지정 곰페르츠 비선형 회귀분석 사례 p.179 ####
# y=Asym*exp(-b2*b3^x)
tt=c(1,2,4,5,6, 8,9,10,13,15, 18,20,24,28,30)
yy=c(1,20,31,34,43, 60,66,75,79,82, 84,85,86,86,87)
nn=nls(yy~SSgompertz(tt,Asym,b2,b3))
summary(nn)
# plot
plot(c(0,30),c(0,90),type = "n",
     main = "Non-linear Regression Plot \n
     Self-Start Gompertz Growth", xlab="tt: 시간", ylab="yy:개체수")
cols=c(4,2)
pchs=c(17,16)
grid()
points(tt,yy,pch=pchs[1],col=cols[1])  # 측정값
points(tt,predict(nn)[1:15],pch=pchs[2],col=cols[2])  #추정값
dd=seq(0,30,0.2)
p1=predict(nn,data.frame(tt=dd))
lines(dd,p1) 
abline(h=coef(nn)[1],lty=2)
abline(h=coef(nn)[2],lty=2)
aa=signif(coef(nn)[1],3)
text(8,84,paste("Asymptotic Line: yy=",aa))
legend("bottomright",legend = c("실험측정값","모형추정값"), col=cols,pch=pchs)

# 9. SSmicmen  Michaelis-Menten Model 비선형 분석사례 p.181 ####
# y=(vm*x)/(k+x) (vm은 목적함수 최대값,K는 vm 50% 에서의 x값)
xx=c(1,2,4,5,6, 8,9,10,13,15, 18,20,24,28,30)
yy=c(1,20,31,34,43, 60,66,75,79,82, 84,85,86,86,87)
nn=nls(yy~SSmicmen(xx,vm,k))
summary(nn)
# plot
plot(c(0,40),c(0,100),type = "n",
     main = "Non-linear Regression Plot \n
     Self-Start Michaelis-Menten", xlab="xx: 투입요소", ylab="yy:반응수준")
cols=c(4,2)
pchs=c(17,16)
grid()
points(tt,yy,pch=pchs[1],col=cols[1])  # 측정값
points(tt,predict(nn)[1:15],pch=pchs[2],col=cols[2])  #추정값
dd=seq(0,40,0.5)
p1=predict(nn,data.frame(xx=dd))
lines(dd,p1) 
abline(h=aa/2,lty=2)
abline(v=coef(nn)[2],lty=2)
aa=signif(coef(nn)[1],3)
text(10.5,90,paste("Asymptotic Line: yy=",aa))
text(14,58,"yy=vm/2")
text(12,0,"k=9.1")
legend("bottomright",legend = c("실험측정값","모형추정값"), col=cols,pch=pchs)

# 10.SSweibull  초기값 무지정 베이블 성장 비선형 회귀분석 사례 p.183 ####
tt=c(1,2,4,5,6, 8,9,10,13,15, 18,20,24,28,30)
yy=c(1,20,31,34,43, 60,66,75,79,82, 84,85,86,85,84)
nn=nls(yy~SSweibull(tt,Asym,Drop,lrc,pwr))
summary(nn)
# plot
plot(c(0,40),c(0,100),type = "n",
     main = "Non-linear Regression Plot \n
     Self-Start Weibull Growth", xlab="xx: 시간", ylab="yy:개체수")
cols=c(4,2)
pchs=c(17,16)
grid()
points(tt,yy,pch=pchs[1],col=cols[1])  # 측정값
points(tt,predict(nn)[1:15],pch=pchs[2],col=cols[2])  #추정값
dd=seq(0,40,1)
p1=predict(nn,data.frame(tt=dd))
lines(dd,p1) 
aa=signif(coef(nn)[1],3)
text(10.5,90,paste("Asymptotic Line: yy=",aa))
abline(h=aa,lty=2)
legend("bottomright",legend = c("실험측정값","모형추정값"), col=cols,pch=pchs)

#---------------- Extended regression Model------------------------------------
# 독립변수가 정량이 아닌 정성변수일 경우, 가변수를 이용 : Dummy regression
# 비대칭(분포) 종속변수 : quantail regression
# 독립변수들 간의 상관성이 높음 : lidge, rasso  regression
# 종속변수의 정규성이 의심스러울 경우 : Box-cox regression
# 오차항의 정규성이 의심스러울 경우 : Robust regression
# 종속변수가 제한된 범위일 경우 : Tobit regression
# 독립변수가 제한된 범위일 경우 : Partial least sq. R.

# 11. Dummy regression 가변수 회귀분석 p.187 ####
# 투입요소1:Kg, 투입요소2:성별(더미), y : 산출량
# test-statistic : F=MSR/MSE, 기각역 F>F(1,n-2/a)
x1=c(22,34,35,36,28,35,46,42,39,25,36,25,38,40,37)
x2=c(1,2,1,2,1,2,1,1,2,2,1,2,1,2,1)
y=c(56,47,55,69,60,57,68,66,55,40,54,42,58,63,59)
k1<-lm(y~x1+as.factor(x2))

# nomal regression
par(mfrow=c(1,2))
plot(x1,y,pch=c("1","2")[x2],main = "Dummy regression1")
ll=lm(y~x1+x2)
abline(ll$co[1]+ll$co[3],ll$co[2],lty=2)
abline(ll$co[1]+1.5*ll$co[3],ll$co[2],lty=1)
abline(ll$co[1]+2*ll$co[3],ll$co[2],lty=2)
text(25,45,"abline for 2(여자)",cex=0.7)
text(25,50,"abline for all(전체)",cex=0.7)
text(25,53,"abline for 1(남자)",cex=0.7)
#  variable의 interaction check
for(i in 1:length(x1)){xx1=x1[(x2<=1)];yy1=y[(x2<=1)]}
for(i in 1:length(x1)){xx2=x1[(x2==2)];yy2=y[(x2==2)]}
ll=lm(y~x1+x2) ; l1=lm(yy1~xx1) ; l2=lm(yy2~xx2)
plot(x1,y,pch=c("1","2")[x2],main = "Dummy regression2")
abline(l1,col=2)
abline(l2,col=4)
abline(ll$co[1]+ll$co[3]*1.5,ll$co[2])
text(25,45,"abline for 2(여자)",cex=0.7)
text(25,50,"abline for all(전체)",cex=0.7)
text(25,53,"abline for 1(남자)",cex=0.7)
# 교호작용 없을 시와 있을 시의 anova 비교 
k2<-step(lm(y~x1*as.factor(x2)))
anova(k1,k2)

# 12. Polymial regression 다항회귀분석 p.191 ####
# 제한적으로 사용됨(독립변수 끼리의 상관성 높을 수 있음), 독립변수가 많으면 무리무리
# 시간에 따른 변화량 조사를 2차 다항 회귀로 함
# test-statistics : T-test
tt=c(1:15)
yy=c(10,12,15,18,22, 24,29,35,40,46, 53,61,70,80,92)
l1=lm(yy~tt)
l2=lm(yy~tt+I(tt^2))
l2
anova(l1,l2)
par(mfrow=c(1,1))
plot(c(0,20),c(0,95),type = "n",main="Polynimial regression Plot",
     xlab ="tt: 시간",ylab="yy: 변화량")
cols=c(1,2,4)
pchs=c(17,16,1)
grid()
points(tt,yy,pch=pchs[1],col=cols[1])
points(tt,predict(l1)[1:15],pch=pchs[2],col=cols[2])
points(tt,predict(l2)[1:15],pch=pchs[3],col=cols[3])
dd=seq(0,20,1)
p1=predict(l1,data.frame(tt=dd))
p2=predict(l2,data.frame(tt=dd))
lines(dd,p1)
lines(dd,p2)
legend("bottomright",legend = c("실험값","선형추정값","다항추정값"),col=cols,pch=pchs)

# 13. Quantile regression 분위 회귀분석 p.194 ####
# 통계량 q=Fy(uq) ; 비대칭 절대오차 최소법 사용
library(quantreg)
data(engel)
attach(engel)
head(engel,2)
taus<-c(.05,.1,.25,.5,.75,.9,.95) # 분위 설정
(f<-coef(rq((foodexp)~(income),tau = taus))) # 분위 회귀분석 수행
lm(formula = foodexp~income) # 선형 회귀분석 수행
#plot
plot(income,foodexp,xlab = "가구수입",ylab="식비",type="n",
     cex=.5,main="Quantile regression Plot")
points(income,foodexp,cex=.5,col="blue")
xx<-seq(min(income),max(income),100)
yy<-cbind(1,xx) %*% f
lines(xx,yy[,2],col="black",lty=4) #0.1
lines(xx,yy[,4],col="red")        #0.5 
lines(xx,yy[,6],col="gray",lty=4) #0.9
abline(lm(foodexp~income),col="blue",lty=2) #선형추정
legend("bottomright",c("90% fit","median fit","mean fit","10% fit"),
       col=c("gray","red","blue","black"),lty = c(4,1,2,4))

# 14. Robust regression p.197 ####
# robust 회귀분석은 종속변수(오차항)의 정규성에 대한 의심,box-cox는  정규화
# 오차항의 분포가 정규보다 꼬리가 길경우 오차항이 이중지수 분포를 따르는 것 가정
x=c(22,34,35,36,28,35,46,42,39,25,36,25,38,40,37)
y=c(50,54,55,69,63,57,68,66,55,47,54,48,58,63,59)
ll=lm(y~x)
summary(ll)
par(mfrow=c(2,2))
plot(ll) #튀는 값을 없애면 R^2가 올라가겠지만, 그게 옳을까? #정규화를 위해?
(lr=line(x,y)) # 로버스트 모형은 튀는 값을 없애고 그리는 선형모형과 비슷하다 R^2=0.95
# plot
par(mfrow=c(1,1))
plot(x,y,xlab = "투입량",ylab="산출량",type="n",main="Robust regression")
grid()
points(x,y,col="green",pch=17)
abline(ll,col="blue",lty=2)
abline(lr,col="red")
legend("bottomright",legend=c("suvey data","Linear fit","Robust fit"),
       col = c("green","blue","red"),lty = c(NA,2,1),pch = c(17,NA,NA))

# 15. Box-Cox regression p.201 ####
# 종속변수의 정규화 lamda!=0,Wi(lamda)=(Yi^lamda - 1)/lamda  or lamda==0,Wi(lamda)=log(Yi)
# Box-cox 변환을 위하여 lamda값부터 추정해야 하는데,MLE로 추정한다.
x=c(22,34,35,36,28,35,46,42,39,25,36,25,38,40,37)
y=c(50,54,55,69,63,57,68,66,55,47,54,48,58,63,59)
ll=lm(y~x)
summary(ll)
par(mfrow=c(2,2))
plot(ll) 
# 잔차항에 문제 있음
library(AID)
boxcoxnc(y)
yy=(y^0.06-1)/0.06
l2=lm(yy~x)
summary(l2)

# 16. Lidge regression p.205 ####
# 최소제곱 추정치에 축소계수(ridge parameter)를 추가해 독립변수 독립성 해결
# lamda(축소계수)를 찾는 방법이 중요한데, GCV 이용
x1=c(22,34,35,36,28,35,46,42,39,25,36,25,38,40,37)
x2=c(25,30,33,33,31,35,42,41,35,20,30,21,30,41,32)
x3=c(51,11,24,21,26,52,56,55,26,59,55,21,18,19,22)
y =c(56,47,55,69,60,57,68,66,55,40,54,42,58,63,59)
xx=cbind(x1,x2,x3)
cor(xx)
cor.test(x1,x2) # x1,x2의 높은 상관성

# lamda trace 방법 : 람다를 0~3까지 0.1씩 증가하여,GCV 최소값 탐색, 
#                   0.9~1.1까지 0.01씩 변화하여,GCV  최소값 탐색
library(MASS)
r1=lm.ridge(y~x1+x2+x3,lambda=seq(0,3,0.1))
r1$GCV[r1$GCV==min(r1$GCV)] # 최소값 찾기
r2=lm.ridge(y~x1+x2+x3,lambda=seq(0.9,1.1,0.01))
r2$GCV[r2$GCV==min(r2$GCV)] # 최소값 찾기
plot(r2,lambda=seq(0.9,1.1,0.01))
# select 방법 : lamda 값을 0~3까지 0.001 GCV 탐색
select(lm.ridge(y~x1+x2+x3,lambda = seq(0,3,0.001)))
#
(r3=lm.ridge(y~x1+x2+x3,lambda = 0.963,Inter=T)) # 능형회귀분석

# 17. Rasso regression p.209 ####
# parameter s를 찾는 것인데, 교차확인법과 일반화 교차확인법이 있다.
# GCV(s)가 최소가 되는 fraction Parameter 추정을 위해서 trace 방법 적용
x1=c(22,34,35,36,28,35,46,42,39,25,36,25,38,40,37)
x2=c(25,30,33,33,31,35,42,41,35,20,30,21,30,41,32)
x3=c(51,11,24,21,26,52,56,55,26,59,55,21,18,19,22)
y =c(56,47,55,69,60,57,68,66,55,40,54,42,58,63,59)
xx=cbind(x1,x2,x3)
cor(xx)
cor.test(x1,x2) # x1,x2의 높은 상관성
# stepwise 회귀 확인
library(lars)
ls=lars(xx,y,type = "stepwise",normalize=F)
summary(ls) # x2변수 1개가 최적모형
summary(lm(y~x2))
# lasso 회귀
(la=lars(xx,y,type = "lasso",normalize = F)) #step 5로 산정
coef(la,s=5,intercept=T) # lasso 회귀계수 ,s=step
(ff=predict.lars(la,xx,s=5,type = "fit")) # lasso 분석결과 

# 18. Tobit regression 토빗 회귀분석 p.213 ####
# 절단된 투입요소 1,2 그리고 산출량이 존재한다
# 산출량이 1~4의 값을 가지는 데이터
x1=c(2,1,1,2,1,1,2,2,1,2,1,2,2,1,2,1)
x2=c(25,16,23,33,31,25,39,41,30,17,20,21,30,41,42,38)
y=c(1.8,1.0,1.7,3.5,3.1,2.2,3.5,4.0,3.0,1.0,1.9,2.2,3.6,4.0,4.0,2.9)
xx=cbind(x1,x2)
library(AER)
lt<-tobit(y~xx,left = 1,right = 4)
summary(lt) # 제약 tobit

# 19. Partial Least Squares Regression Model PLS 회귀 p.215 ####
# 5가지 주거항목과 그에 대한 만족도
library(pls)
x1=c(45,57,43,67,58, 39,57,35,58,57, 67,48,65,84,67, 90,35,57) # stablity
x2=c(56,68,69,79,55, 58,79,46,69,82, 84,52,74,68,48, 68,39,58) # healthy
x3=c(56,68,79,73,86, 54,58,79,47,69, 57,68,79,57,58, 57,58,58) # utility
x4=c(63,85,83,45,54, 56,67,47,79,80, 75,53,57,67,46, 58,84,52) # finess
x5=c(53,64,58,68,43, 48,63,55,70,88, 77,53,66,75,67, 87,56,90) # effectivity
yy=c(11,23,33,45,54, 45,56,22,45,65, 66,84,67,32,78, 85,56,90) # satisfy
ps<-plsr(yy~x1+x2+x3+x4+x5, ncomp=2,validation="CV")
summary(ps)
# 분석결과
names(ps)
ps$coefficients

# 20. Linear Mixed Regression Model  p.217 ####
# 
library(lme4)
x1=c(22,34,35,36,28, 35,46,42,39,25, 36,25,38,40,37)
x2=c(25,16,23,33,31, 25,39,41,30,19, 20,21,30,41,42)
x3=c(51,11,24,21,26, 52,56,55,26,59, 55,21,18,19,22)     
y=c(56,47,55,59,60, 57,68,66,55,40, 54,42,58,63,59)
x4=as.factor(c(1,1,1,1,1, 1,1,1,2,2, 2,2,2,2,2))
mm=lmer(y~x1+x2+x3+(1|x4))
summary(mm)
