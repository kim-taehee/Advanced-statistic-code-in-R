#######################################################
# liner Regression Analysis
# General linear Regression Analysis
#######################################################
# 기본 회귀모형 이론
# test statistic : F = MSR/MSE
# check    Multi-Colinearity   by VIF = 1 / (1-R^2) < 10
# DW statisctics 로 잔차 독립성 검정
# 절편이 없을때는 (no intercept) lm=(y~x -1)  ------------------어떤 경우에 절편이 없나
#--------------------------------------------------------
# 1. simple linear regression Model  p.102 ####
x=c(22,34,35,36,28,35,46,42,39,25,36,25,38,40,37)
y=c(56,47,55,69,60,57,68,66,55,40,54,42,58,63,59)
summary(lm(y~x))

a1=signif(lm(y~x)$coefficient[1],digits = 3)
b1=signif(lm(y~x)$coefficient[2],digits = 3)
a2=0
b2=signif(lm(y~x-1)$coefficient[1],digits = 3)
plot(x,y,main = "plot for liner regression")
abline(a1,b1) # 정규전형
abline(a2,b2) # 원점 회귀 - 잔차 x
text(29,40,paste("y=",b2,"x"))
text(28,55,paste("y=",a1,"+",b1,"x"))
grid()

lsfit(x,y) #최소자승법과 선형대수QR분해법으로 구하는 모델

# 발표용 4분할 자료 (4 vision for PT)
par(mfrow=c(2,2)) # 화면분할
?par #파레트 palette
plot(lm(y~x),main = "simple linear plot") #plot 제목설정
require(psych) # pcych package
pairs.panels(xx,main="pairs Diagram") # pair diagram 구성

#측정자료의 검토(estimate test)
m1=lm(y~x)
i1=influence.measures(m1) # 해당함수
par(mfrow=c(1,1))
plot(x,y,main="Simple Linear regression \n influence measures")
abline(m1); points(x[i1$is.inf], y[i1$is.inf],pch=20,col=2) #???
i1

#잔차의 검토(residual test)
m1=lm(y~x)
(rr=round(rstudent(m1,infl = lm.influence(m1)),3))

mo=lm(y~x)  #선형모형구성
nx=data.frame(x=c(seq(21,48,0.5))) #새로운 X자료
p1=predict(mo,nx,interval = "prediction")# 예측자료
p2=predict(mo,nx,interval = "confidence")# 신뢰구간
plot(x,y,main = "Linear Regression pre-Plot")

matplot(nx$x,cbind(p1),lty=c(1,2,2),type="l",col=2,xlab="",ylap="",add=T) #예측자료 plot
plot(x,y,main = "Linear Regression confi-Plot")        
par(new=TRUE)        
?par
matplot(nx$x,cbind(p2),lty=c(1,2,2),type="l",col=2,xlab="",ylap="",add=T) #신뢰구간 plot 
        

# 2. Multi linear regression Model  p.110 ####
a1=c(22,34,35,36,28,35,46,42,39,25,36,25,38,40,37)
a2=c(25,16,23,33,31,25,39,41,30,19,20,21,30,41,42)
a3=c(51,11,24,21,26,52,56,55,26,59,55,21,18,19,22)
y=c(56,47,55,69,60,57,68,66,55,40,54,42,58,63,59)
aa=matrix(c(a1,a2,a3),ncol = 3,nrow = 15)
summary(lm(y~aa))


# standardzation index, 표준화 계수 및 예측치 선정
# scale()
# 다중공선성은 car()의 vif

# mle (MAx likehood estimate) 선형 중회귀 우도검정
mo1=lm(y~a1+a2+a3) #모형1
mo2=lm(y~a1+a2)   #모형2
lamda=-2*(logLik(mo2)-logLik(mo1)) #lamda
1-pchisq(lamda,1) #유의확률 0.5보다 커서 별 차이 없는 것으로 나옴
AIC(mo1)

# Durbin-Watson test 자기상관성
a1=c(22,34,35,36,28,35,46,42,39,25,36,25,38,40,37)
a2=c(25,16,23,33,31,25,39,41,30,19,20,21,30,41,42)
a3=c(51,11,24,21,26,52,56,55,26,59,55,21,18,19,22)
y=c(56,47,55,69,60,57,68,66,55,40,54,42,58,63,59)
mo1=lm(y~a1+a2+a3) #모형1
require(lmtest)
dwtest(mo1)

# 중회귀 회귀분석 분석사례
# AIC로 산정한다.
a4=c(10,11,21,22,13,15,24,10,16,14,35,16,12,14,10)
mo2=lm(y~a1+a2+a3+a4)
lm3=step(mo2) #변수고르기
mle.aic(mo2)

#--------------------------------------------------------------------------
# GLM은 CLM의 가정(정규성, 등분산성) 등을 가정하지 않음
# 종속변수가 비정규분포(이항분포,감마분포,포아송분포) 등을 따르는 경우 적용
#  GLMM(general linear mixed model)은 개체적 장소적 모형고려

#  3. Gaussian regression   unbias 불편성 p.128 ####
b1=c(22,34,35,36,28,35,46,42,39,25,36,25,38,40,37)
b2=c(25,16,23,33,31,25,39,41,30,19,20,21,30,41,42)
b3=c(51,11,24,21,26,52,56,55,26,59,55,21,18,19,22)
y=c(56,47,55,69,60,57,68,66,55,40,54,42,58,63,59) 
(bb=matrix(c(b1,b2,b3),ncol = 3,nrow = 15))
glm(y~bb)

# 4. lostic regression  p.129  ####
# 독립변수 binominal,종속변수 binominal, 연계함수는 logit 
ii=c(32,10,8,30)
xx=gl(2,1,4)
yy=gl(2,2,4)
gg=glm(yy~xx,family = binomial,weights =ii )
(bb=round(prop.table(matrix(ii,nrow=2),1),2)) # 가로 or 세로로 확률합1
summary(gg)

pa=1/(1+exp(gg$coefficients[1])) # 1/(1+exp(0))
pb=1-pa
pc=1/(1+exp(gg$coefficients[1]+gg$coefficients[2])) # 1/(1+exp(1))
pd=1-pc
odds=(pd/pc)/(pb/pa) #오즈비
odds

r1=1-exp((gg$deviance-gg$null.deviance)/gg$df.null)
r2=r1/(1-exp(-gg$null.deviance/gg$df.null))
r2  #  Faraway 일반화선형 회귀계수 R^2

# 5. lostic regression  p.131 ####
# 독립변수 level 3 , 종속변수 binominal, 연계함수 logit
ii=c(14,11,4,6,9,16)
xx=c(0,1,2,0,1,2)
yy=c(0,0,0,1,1,1)
(bb=round(prop.table(matrix(ii,nrow=3),1),2)) # 가로 or 세로로 확률합1
gg=glm(yy~xx,family = binomial,weights =ii )
summary(gg)
pb=gg$fitted.values[1]; pa=1-pb # 반응이 있을때(y=1), 연계함수에 x[1]값 대입
pd=gg$fitted.values[2]; pc=1-pd # 반응이 있을때(y=1), 연계함수에 x[2]값 대입
pf=gg$fitted.values[3]; pe=1-pf # 반응이 있을때(y=1), 연계함수에 x[3]값 대입

r1=1-exp((gg$deviance-gg$null.deviance)/gg$df.null)
r2=r1/(1-exp(-gg$null.deviance/gg$df.null))
r2  #  Faraway 일반화선형 회귀계수 R^2

odds2=((9/11)/(6/14)) # 0대비 x가 1증가 오즈비
odds2
odds3=((16/4)/(6/14)) # 0대비 x가 2증가 오즈비
odds3

 

# 6. losgitic regression  p.133  ####
# 독립변수  level 3 , 종속변수 binominal, 연계함수 logit, 지역변수 2
ii=c(14,11,4,12,10,7,6,9,16,8,10,13)
x1=c(rep(0:2,4))
x2=rep(c(rep(0,3),rep(1,3)),2)
yy=c(rep(0,6),rep(1,6))
(bb=prop.table(matrix(ii,nrow = 6),1)) # 가로 or 세로로 확률합1
gg=glm(yy~x1*x2,family = binomial,weights =ii )
summary(gg)

r1=1-exp((gg$deviance-gg$null.deviance)/gg$df.null)
r2=r1/(1-exp(-gg$null.deviance/gg$df.null))
r2  #  Faraway 일반화선형 회귀계수 R^2

#필요없는 변수 제거
ggs=step(gg,direction = "both")
# 단순화

# 7. losgitic regression  p.137 ####
# 독립변수 수치형 , 종속변수 binominal, 연계함수 logit
aa=c(0,1,3,5,7, 21,31,51,71,81, 91,94,95,96,97)
bb=c(0,0,0,1,2, 5,10,25,35,60, 82,90,92,95,97)
cc=seq(21,49,2)
dd=data.frame(aa,bb,cc) # data set
mo<-glm(cc~bb/aa,family = gaussian)
m1<-glm(cbind(bb,aa-bb)~1,family = binomial)
m2<-glm(cbind(bb,aa-bb)~cc, family = binomial(link = "logit"))
summary(m2)

# analysis plot
plot(c(21,49),c(0,1),type = "n",main = "logistic regression plot",
     xlab = "cc:시약투여량",ylab="probablity: 발생확률")
points(cc,m2$fit,pch=16,col=2)
points(cc,m2$fit,type = "l")
points(cc,m2$y,pch=17,col=4)

# 8. Ordinal Multiple Logistic Model (순서형 다항 로지스틱 모델)  p.139 ####
# 독립변수 순서형, 종소변수 binominal, 연계함수 logit
x1=c(50,48,35,31,45, 48,38,41,33,43, 22,11,25,25,23,
     33,25,29,30,32, 39,45,37,35,47, 28,29,30,31,19)
x2=c(35,31,38,35,42, 40,39,45,42,55, 38,35,42,49,36,
     35,38,37,42,30, 39,48,59,55,49, 39,45,40,49,50)
yy=factor(rep(c(2,1,3),each=5,times=2),ordered=T) #순서형변수 설정
          
library(MASS)
ga<-polr(yy~x1+x2, method = c("logistic"))
gp<-polr(yy~x1+x2, method = c("probit"))
gl<-polr(yy~x1+x2, method = c("loglog"))
gc<-polr(yy~x1+x2, method = c("cloglog"))
print(c(ga$deviance,gp$deviance,gl$deviance,gc$deviance))
summary(ga) #각 모형을 summary 했을 때 잔차편차의 최소값을 찾아라
pairs(profile((ga)))
# 모델의 결과 자체는 별로임
summary(update(ga,method="probit",Hess=T)) #분석모형을 보정, 결국 프로빗 모형

ga22<-stepAIC(ga,~.^2) # AIC를 최소로하기 위한 보정, 결과적으로 교호작용이 있어야 설명력 증가
anova(ga,ga22) # 잔차편차 확인

# 9. Probit regression Model  p.142 #### 
# 독립변수 2개(이항 1개,명목형 1개),종속변수 binominal, 연계함수 probit
# 프로빗과 로지스틱의 차이는? 잔차항의 분포를 정규라 보면 프로빗, 아니면 로지스틱
y1=c(0,5,11,18,24,27, 1,10,15,23,29,30)
x1=rep(1:6,2)
x2=factor(rep(c("남","녀"),c(6,6)))
yy=cbind(y1,30-y1)
bb=round(prop.table(matrix(yy, nr=12),1),2)
df<-data.frame(cbind(x1,x2,yy,bb))
df
gg=glm(yy~x1*x2,family = binomial(link="probit"))
summary(gg)
(gg$fitted.values[1]) # fitted value는 y가 1일때의 [x]의 추정확률,bb는 자료비율 p.143
#  gg$fitted.values[x]  는 p=화이(b0+bx1+bx2)이고 p=F(b0+bx1+bx2)이다, F는 정규누적확률밀도함수

# odds graph
plot(c(1,32),c(0,1), type = "n", log = "x", main = "Probit regression Plot"
     ,xlab = "x1",ylab = "y1")
grid(5,5)
text(2^x1,y1/30,as.character(x2))
lines(2^dd,predict(gg,data.frame(x1=dd),x2=factor(rep("남",length(id)),
        levels=levels(x2)),type="response"))

# 10. poisson regression Model p.145    2개 수준,2개 명목척도변수 #### 
#  포아송 회귀는 잔차항이 포아송을 따르나?No
#  독립변수가 명목,순서형 이고, 종속변수가 작은확률(포아송분포)
cc=c(5,45,11,39)
x1=rep(1:2,2)
x2=factor(rep(c("남","녀"),c(2,2)))
gg=glm(cc~x1*x2, family = poisson(link="log"))
p1=exp(gg$coefficients[1]+gg$coefficients[2])
p2=exp(gg$coefficients[1]+2*gg$coefficients[2])
p3=exp(sum(gg$coefficients))
p4=exp(sum(gg$coefficients)+gg$coefficients[2]+gg$coefficients[4])
p5=exp(gg$coefficients[4])

# 11. poisson regression Model  p.147  3개 수준, 2개 명목척도변수 (level 2,level 3) ####
pp=c(15333,12351,10331,  14523,11237,9721)
cc=c(32,21,14,  41,28,18)
x1=factor(rep(1:3,2)) # factor 1 = income 
x2=factor(rep(c("남","녀"),c(3,3))) # factor 2 = sex
print(cbind(x1,x2,cc,pp))
gg=glm(cc~x1+x2+offset(log(pp)), family = poisson())
gg
gg$fitted.values # each result 각 수치별 결과 값

# 12. Poisson regression Model  p.149 3개 수준,2개 명목척도변수 (level 3,level 3) ####
counts<-c(12,19,25,21,22,23,24,28,33)
treatmenta<-gl(3,1,9)
treatmentb<-gl(3,3)
dd<-data.frame(counts,treatmenta,treatmentb)
# par(mfrow=c(1,2)) 이미 페어스는 안되더라
# par(mfrow=c(1,1)) 초기화
pairs(dd, panel = panel.smooth)
plot(treatmenta,counts,type="n",main="Poisson Regression Plot",xlab="treatmenta",ylab="counts")
legend("bottomright",legend = c("treatmentb=1","treatmentb=2","treatmentb=3"),
       pch=c(15,16,17),col=c(1,2,3))
points(treatmenta[treatmentb==1],counts[treatmentb==1],col=1,pch=15)
points(treatmenta[treatmentb==2],counts[treatmentb==2],col=2,pch=16)
points(treatmenta[treatmentb==3],counts[treatmentb==3],col=3,pch=17)
# Choose best AIC  최적 분석모형 설정
model2<-step(glm(counts~treatmenta*treatmentb, family = poisson()))
model2
# 과분산 함수 p.151
odisPossion<-function(model,level=0.95){ 
  if((level<0) || (level>1)){cat("level:error\n")}
  else { if(model$family$family=="poisson"){
          Y<-model$y
          pred<-model$fitted
          area.over<-qpois(1-(1-level)/2,lambda = pred) #lamda의 포아송의 상 유의수준범위
          area.under<-qpois(0+(1-level)/2,lambda = pred) #lamda의 포아송의 하 유의수준범위
        } else if(model$family$family=="binomial"){
          Y<-model$model[1][,1][,1]
          N<-model$model[1][,1][,2]
          YN<-Y+N
          pred<-model$fitted*YN
          area.over<-qbinom(1-(1-level)/2,size = YN,prob = pred/YN) #YN의 포아송의 상 유의수준범위
          area.under<-qbinom(0+(1-level)/2,size=YN,prob = pred/YN) #YN의 포아송의 하 유의수준범위
        }
  info<-data.frame(Y,pred,area.over,area.under)        
  info<-info[order(info$pred),]
  with(info,{plot(pred,Y,xlab="predictive value",
                  main="Model Check: Over Dispersion",
                  ylab = "Real value & Conf.Interval")
            points(pred,pred,type="l")
            lines(pred,area.over,lty=2,col=2)
            lines(pred,area.under,lty=3,col=3)
                  })
  legend("bottomright",legend = c("Fitted Line","Upper Limit","Lower Limit"),
         lty=c(1,2,3),col = c(1,2,3)
         )
  samples<-length(Y)
  outers<-length(Y[Y<area.under|area.over<Y])
  cat(c("cofidence interval level:",level*100,"%\n"))
  cat(c("outers:",outers,"in",samples,"samples\n"))
    }
}
odisPossion(model2)

# 13. Complog regression Model  12개수준 - 2개 정성변수 p.152 ####
x1=seq(100,430,30)
x2=c(30,35,35,35,31,20,25,25,25,30,30,30)
y1=c(1,5,3,5,10,6,15,19,22,24,28,29)
y2=x2-y1
yy=cbind(y1,y2)
gg=glm(yy~x1,family = binomial(link = "cloglog"))
summary(gg)
r1=1-exp((gg$deviance-gg$null.deviance)/gg$df.null)
r2=r1/(1-exp(-gg$null.deviance/gg$df.null))
gg$fitted.values #검증하고 싶으면 다른데이터르 넣어서 원모델 gg$fitted 와 비교하면 댐

#plot
plot(c(100,430),c(0,1),type = "n",main = "Complog Regression Plot", 
     xlab="x1:원료투입량",ylab="Probablity:발생확률")
points(x1,y1/(y1+y2),pch=17,col=4)
points(x1,gg$fitted.values,pch=16,col=2)
dd=seq(100,430,10)  # 예측을 위한 검증 파일 생
p1=predict(gg,data.frame(x1=dd,type="")) # 예측값으로 dd를 넣음
lines(dd,(1-exp(-exp(p1)))) # link function 그려보기 ,  잘 그려지네 
legend("bottomright",legend = c("실험측정값","모형추정값"),col=c(4,2),pch=c(17,16))

# 14. GLM for Covariance  공변량 일반화 회귀분석 p.155 ####
# 독립변수는 정량,정성 변수,종속변수는 정량변수
x1=c(22,34,35,36,28, 35,46,42,39,25, 36,25,38,40,32, 22)
x2=gl(2,8,16)
y=c(46,57,55,59,40, 53,68,66,55,40, 54,42,58,63,59, 48)
s1<-summary(lm(y[x2==1]~x1[x2==1])) #지역 요소인 x2가 1인 경우
s2<-summary(lm(y[x2==2]~x1[x2==2])) #지역 요소인 x2가 1인 경우

# 15. Gamma regression #### 
# 종속변수가 독립변수군에 조건화하여 감마분포를 따를 경우 적용
xx=seq(5,50,5)
yy=c(291,108,41,28,23,30,18,17,16,15)
plot(log(xx),yy,type = "l")
library(MASS)
gm=glm(yy~log(xx),family = "Gamma")
summary(gm)

# 16. GLM for nagative binomial regression) 음이항 회귀분석  p.159 ####
# 음이항 회귀번석은 종속변수 y가 특정사상이 일어날 때까지의 시행횟수
# x에 대한 회귀분석으로서, 음이항 분포는 다음과 같이 요약된다.
# 이러한 발생확률의 변수변환을 적용하면 선형함수가 도출되며, 함수식의 계수들은  MLM로 추정된다 
y1=c(14,13,1,13,11,5,76,80,140,75,14,57)
y2=c(31,13,1,48,11,5,165,93,144,462,168,175)
age=factor(rep(0:1,each=6))
sex=factor(rep(0:1,each=3,times=2))
class2=factor(rep(c(0,1,0),times=4))
class3=factor(rep(c(1,0,0),times=4))
rate=y1/y2*100
(vg=glm.nb(rate~age+sex+class2+class3))
exp(vg$coefficients)  #생존률 식
exp(confint(vg)) # ????
