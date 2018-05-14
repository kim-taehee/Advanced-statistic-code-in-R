# 생존분석에는  두 개 시점사이에 발생되는 사실만 관측한 자료로서,
# '우측 중도절단 자료'와 '구간 중도 절단자료'가 있다.
# 
# 구조방정식(SEM): 연구자가 구상한 분석모형으로 분산분석,회귀분석,요인분석등을 구축하고
# 타당성과 검정을 수행하는 방법이다.(feature engineering 이다) 
# a. 변수의 인과관계 : 직접인과관계, 간접인과관계, 
# 허위인과관계, 상호관계, 분석불가, 조절인과 등으로 나누어 진다.
# b. 적합성 평가 : 회귀처럼 잔차합으로 본다. 
#  절대 적합지수(AFM) : 분석모형 전반적인 적합도 평가 (GFI,AGFI,AIC)
#  증분 적합지수(IFM) : 초기모형에 대한 수정 적합도  (NFI,NNFI)
#  간명 적합지수(PFM) : 모형의 적합성과 간명함의 정도  (AIC) 
#----------------------------------------------------------------------------
# 1. 중도 절단 자료의 생존기간 산정에 대한 분석사례 (survfit) p.420 ####
# 다음의 중도 절단 자료를 이용하여 생존시간 산정하시오

library(survival)
dd=data.frame(time=c(100,200,300,400,400,500),
              censor=c(1,0,1,1,0,1),
              treat=rep("Q",6))
ss=survfit(formula = Surv(time,censor)~1, data=dd, type="kaplan-meier")
summary(ss)
# plot
plot(ss,main="Survival Time Plot by Kaplan Meier",
     xlab = "Time (day)",ylab = "Rate of Event Non-outbreak")
text(ss$time[2], ss$surv[3]-0.07,"(lower 95% CI)",cex = 0.8)
text(ss$time[2], ss$surv[2]-0.05,"Rate")
text(ss$time[2], ss$surv[1]+0.14,"(upper 95% CI)",cex = 0.8)
s2=survfit(Surv(time,censor)~treat,data=dd,conf.int=F,type="fleming-harrington")
s3=survfit(Surv(time,censor)~treat,data=dd,conf.int=F,type="fh2")

# 2. 중도 절단 자료의 생존시간 차이에 대한 검정의 분석사례 p.424 ####
# 30명에 대한 중도절단 자료 중, 치료방법과 성별에 따른 생존분석
time=c(100,200,300,350,400,450,500,550,600,700,750,800,850,900,1000,
     100,200,250,300,350,400,450,500,550,600,650,700,800,850,900) # 생존시간
sex=c(1,1,1,1,1,2,1,2,1,2,1,2,2,2,2,
     1,1,1,1,2,1,2,1,2,1,2,2,2,2,2) # 성별
xx=c(22.2,21.7,20.4,18.5,20.5,18.1,19.9,17.4,18.5,19.5,15.4,14.7,17.2,16.4,15.7,
     21.3,22.6,20.3,19.3,20.2,21.3,16.1,19.8,14.7,19.5,20.3,14.2,13.1,15.3,13.7) # 요소량
treat=c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
     2,2,2,2,2,2,2,2,2,2,2,2,2,2,2) # 처리
censor=c(0,0,0,1,0,1,0,1,1,1,1,1,1,1,1,
         0,0,0,0,0,0,1,0,1,0,0,1,1,1,1)
dd=data.frame(time,sex,xx,treat,censor)
ss=survfit(formula = Surv(time,censor)~treat,type="kaplan-meier")
ss2=survfit(formula = Surv(time,censor)~sex,type="kaplan-meier")
summary(ss)
# 분석결과 검정
survdiff(Surv(time)~treat,data = dd) # 2개의 생존분석 함수 차이 검정 
survdiff(Surv(time)~sex,data = dd) # 2개의 생존분석 함수 차이 검정 
plot(ss,main="Survival Time plot by Treat",lty = 1:2,
     xlab = "Time (day)",ylab = "live rate")
legend("bottomleft",legend=c("treat A","treat B"),lty = 1:2)
plot(ss2,main="Survival Time plot by Sex",lty = 1:2,
     xlab = "Time (day)",ylab = "live rate")
legend("bottomleft",legend=c("male","female"),lty = 1:2)


# 3. cox hazard regression  p.424 #### 비모수 검정
cc=coxph(Surv(time,censor)~xx+as.factor(sex)+as.factor(treat),data = dd) # 요인별 찾기 
summary(cc)
# 위험도 비례성 검토
(zz=cox.zph(cc))
library(MASS)
cc2=coxph(Surv(time,censor)~(xx+sex+treat)^2,data = dd)
stepAIC(cc2)

#--------------------------------------------------------------------------------
# 4. 구조방정식의 경로분석에 대한 분석사례  p.443 ####
# 거리 →  시간
#       ↘  ↓
#         비용
#  안전 ↗
library(sem)
cost=c(30,36,35,39,42, 44,51,52,55,61, 62,63,65,67,69)
distance=c(40,45,48,57,62, 66,79,80,81,88, 89,90,93,95,95)
wasteTime=c(30,33,36,39,40, 42,47,52,55,59, 61,62,63,65,65)
safety=c(11,18,15,14,12, 14,13,17,15,15, 21,24,25,36,22)
dd=cor(cbind(cost,distance,wasteTime,safety))
mo1=specify.model() #직접 경로 설정
ss=sem(mo1,S=dd,N=15)  #모델 분석
summary(ss) # 모델 검정통계량
modIndices(ss) # 모델 변화
mo2=specify.model()
library(psych)
dd2=data.frame(cost,distance,wasteTime,safety)
(mm=mediate(y="cost",x="wasteTime",m=c("safety","distance"),data = dd)) # "  있어야함
setCor.diagram(setCor(y="cost",x=c("wasteTime","safety","distance"), dd,std=FALSE))

# 5. 구조방정식에 따른 회귀분석 사례 p.449 ####
cost=c(30,36,35,39,42, 44,51,52,55,61, 62,63,65,67,69)
distance=c(40,45,48,57,62, 66,79,80,81,88, 89,90,93,95,95)
wasteTime=c(30,33,36,39,40, 42,47,52,55,59, 61,62,63,65,65)
safety=c(11,18,15,14,12, 14,13,17,15,15, 21,24,25,36,22)
dd=cor(cbind(cost,distance,wasteTime,safety))
mo=specify.model()
ss3=sem(mo,S=dd,N=15)
std.coef(ss3)
# plot
semPaths(ss3,"std",nCharNode=4,style="lisrel",
         rotation=2,edge.colo="dark gray",fade=F) # std 표준화계수 
title("Path diagram by regrssion \n weighted type, style = lisrel")

# 6. 구조방정식에 따른 요인분석 p.453 ####
cost=c(30,36,35,39,42, 44,51,52,55,61, 62,63,65,67,69)
distance=c(40,45,48,57,62, 66,79,80,81,88, 89,90,93,95,95)
wasteTime=c(30,33,36,39,40, 42,47,52,55,59, 61,62,63,65,65)
safety=c(11,18,15,14,12, 14,13,17,15,15, 21,24,25,36,22)
dd=cor(cbind(distance,wasteTime,safety))
mo=specify.model()
ss4=sem(mo,S=dd,N=15)
std.coef(ss4)
library(semplot)
fscores(ss4) # 확인적 요인분석 

# 7. 구조방정식의 MIMIC 모형 p.458 ####(Multiple indicator multiple cause model)
# 자료문제로 페이지만 참조 


# 기타 모형도 모델구조만 잘 만들어 주면 된다 -끝-
