#----------------------------------------------------------------------------
# PCA- principal COmponent Analysis
# Discriminant Analysis
# Factor Analysis
#----------------------------------------------------------------------------
#
#  PCA 는 회귀분석과 달리 정보손실 최소화를 목적으로 하며, 최신 머신러닝과도 함께 쓰인다
#  1. 정보손실량 최소화 방법 :  |a2A+a1B+a0|/sqrt(a2^2+a1^2) = 점과 직선의 최단거리
#                              위의 식에서 분모를 1로 만드는 극한함수식 F는 랑그리지함수식
#                              F를 편미분하면 sqrt(a2^2+a1^2)=1이 된다
#  2. 주성분 부산 최대화 방법 : 수학식은 위와 같다.
#  3. 주성분 분석의 해석  : (독립변수 분산)=(주성분분산)+(정보손실량제곱합)/자유도

# 1. 정량 4변수 prcomp p.227 ####
x1=c(26,46,57,36,57, 26,58,37,36,56, 78,95,88,90,52, 56) # 국어
x2=c(35,74,73,73,62, 22,67,34,22,42, 65,88,90,85,46, 66) # 영어
x3=c(35,76,38,69,25, 25,87,79,36,26, 22,36,58,36,25, 44) # 수학
x4=c(45,89,54,55,33, 45,67,89,47,36, 40,56,68,45,37, 56) # 과학
xx=cbind(x1,x2,x3,x4)
colnames(xx)=c("국어","영어","수학","과학")
(pp=prcomp(xx))
summary(pp) # pc1,pc2가 cumulative prop. 의 80%가 넘음 
# 분석결과 pc1=0.61(x1)+0.72(x2)+0.26(x3)+0.21(X4) : 어학능력
#          pc2=-0.39(x1)-0.09(x2)+0.74(x3)+0.54(x4) : 수리능력
plot(pp,xlab="Principal Component",main = "Variance by Prin.Comp")
biplot(pp,main="Prin. Compo.1 / Prin. Compo.2")

# 2. 정량 4변수 상관계수로 분석 p.230 ####
x1=c(26,46,57,36,57, 26,58,37,36,56, 78,95,88,90,52, 56) # 국어
x2=c(35,74,73,73,62, 22,67,34,22,42, 65,88,90,85,46, 66) # 영어
x3=c(35,76,38,69,25, 25,87,79,36,26, 22,36,58,36,25, 44) # 수학
x4=c(45,89,54,55,33, 45,67,89,47,36, 40,56,68,45,37, 56) # 과학
xx=cbind(x1,x2,x3,x4)
cc=cor(xx)
(pp2=princomp(cc,scale. = T))

# 3. 정량적2 , 정성적 2변수 prcomp p.232 ####
x1=c(26,46,57,36,57, 26,58,37,36,56, 78,95,88,90,52, 56) # 국어
x2=c(35,74,73,73,62, 22,67,34,22,42, 65,88,90,85,46, 66) # 영어
x3=c(1,2,2,1,2, 1,2,1,1,1, 2,2,2,1,1,2)
x4=c(1,1,1,1,1, 1,1,1,2,2, 2,2,2,2,2,2)
xx2=cbind(x1,x2,x3,x4)
colnames(xx2)=c("국어","영어","보강","성별")
(pp3=prcomp(xx2,scale.= T))
summary(pp3)
# 분석결과 pc1=0.66(x1)+0.51(x2)-0.17(x3)+0.51(X4) : 학업능력
#          pc2=-0.21(x1)+0.25(x2)+0.52(x3-0.8(x4) : 호기심능력
pp$x
plot(pp3,xlab="Principal Component",main = "Variance by Prin.Comp")
biplot(pp3,main="Prin. Compo1. /Prin. Compo2.")

# 4. Principal Component regression 주성분 회귀분석 분석  p.235 ####
x1=c(26,46,57,36,57, 26,58,37,36,56, 78,95,88,90,52, 56) # 국어
x2=c(35,74,73,73,62, 22,67,34,22,42, 65,88,90,85,46, 66) # 영어
x3=c(35,76,38,69,25, 25,87,79,36,26, 22,36,58,36,25, 44) # 수학
x4=c(45,89,54,55,33, 45,67,89,47,36, 40,56,68,45,37, 56) # 과학
y=c(25,50,42,45,33, 25,54,51,41,49, 58,72,87,77,48, 63) # 종합(다른과목)
ll=lm(y~x1+x2+x3+x4)
summary(ll)
# but cor-test
cor(xx)
cor.test(x1,x2)
cor.test(x3,x4)
step(ll)
#독립변수를 유지하기 위해 주성분분석
xx=cbind(x1,x2,x3,x4)
(pp=prcomp(xx,scale. =T) ) #영향력 체크
summary(pp) #주성분 확인
lp=lm(y~pp$x[,1]+pp$x[,2]) #pp의 1요인과 2요인으로 회귀
summary(lp)

# 5. kernel 비선형 주성분 분석 p.239 ####
x1=c(26,46,57,36,57, 26,58,37,36,56, 78,95,88,90,52, 56) # 국어
x2=c(35,74,73,73,62, 22,67,34,22,42, 65,88,90,85,46, 66) # 영어
x3=c(1,2,2,1,2, 1,2,1,1,1, 2,2,2,1,1,2)
x4=c(1,1,1,1,1, 1,1,1,2,2, 2,2,2,2,2,2)
xx2=cbind(x1,x2,x3,x4)
colnames(xx2)=c("국어","영어","보강","성별")
kk=kpca(xx,kernel="rbfdot",feature=4)
summary(kk)

#--------------------------------------------------------------------------------------
#  판별분석은 2단계 수행되며 모집단은 정규성,불편성, 등분산성을 만족으로 가정
#  1단계 : y에 대하여 독립변수들의 함수관계를 토대로 집단구분 혹은 차원축소가 가능한가
#  2단계 : 집단구분 후 집단으로 판별해주는 것
#  분석방법에 따른 분류
#  a. 선형판별분석 : 판별함수가 독립변수들의 선형결합
#  b. 비선형판별분석 : 2차함수,로지스틱,마할라노비스 거리 등으로 구분 
# 선형판별함수는 주성분분석과 판별함수가 같다,유의성은 Wilk's Lambda 값으로 결정
# 비선형 판별함수는 분산 공분산행렬이 같은가 확인하고나서 2차판별 함수 적용

# 6. lda 선형판별 p.246 ####
# 독립변수의 등분산성을 가정하고 있다, 등분산 먼저 검정 
x1=c(41,38,27,36,25,48,40, 32,10,35,22,22,15,19,25)
x2=c(45,21,26,23,30,49,35, 28,23,42,18,26,22,30,35)
y=c(2,2,2,2,2,2,2 ,1,1,1,1,1,1,1,1)
library(MASS)
(ld=lda(y~x1+x2))
# 분석고정에서 판별식의 상수항은 선형 판별분석의 계수에 평균값을 대입 
(aa=apply(ld$means%*%ld$scaling,2,mean))
predict(ld) # 판별결과
# 판별결과, 판별률은 입력된 자료y와 predcit class와 촬영
tt<-table(y,predict(ld)$class)
round(prop.table(tt),3)
# 판별예측
new=data.frame(x1=c(20),x2=c(40))
predict(ld,new)

# ROC 및 판별그래프
library(caTools)
xx=data.frame(x1=c(x1),x2=c(x2))
colAUC(xx,y,plotROC = T) # 민감도와 ROC
# linear Discrminent plot
plot(x1,x2,pch=paste(y),main = "Linear Discrminent Plot")
abline(aa/ld$scaling[2],-ld$scaling[1]/ld$scaling[2])  #???
legend("bottomright",c("1=무반응","2=반응"))
# cross validation
(lc=lda(y~x1+x2,CV=T))
# 판별분석 그래프
library(klaR)
drawparti(y,x1,x2,method = "lda",col.wrong = "black",xlab = "x1",ylab="x2")
legend("bottomright","1=무반응 2=반응",cex=0.7)

# 7. qda 비선형 판별분석의 분석사례 p.252 ####
x1=c(51,48,37,30,45,48,40, 22,11,25,15,23,30,19,30)
x2=c(45,31,28,27,50,40,37, 38,35,42,49,36,33,40,37)
y=c(2,2,2,2,2,2,2, 1,1,1,1,1,1,1,1)
xx1=cbind(x1[y==1],x2[y==1]) # 결과 1을 가지는 투입요소들
xx2=cbind(x1[y==2],x2[y==2]) # 결과 2를 가지는 투입요소들
s1=cov(xx1)
s2=cov(xx2)
n1=length(xx1); n2=length(xx2); p=2 # p는 뭐지? 결과 2개?
ss=((n1-1)*s1+(n2-1)*s2)/(n1+n2-2) #공분산수식
p1=(n1+n2-2)*log(abs(det(ss)))-(n1-1)*log(abs(det(s1)))-(n2-1)*log(abs(det(s2))) # ??
p2=1-(1/(n1-1)+1/(n2-1)-1/(n1+n2-2)) *  (2*p^2+3*p-1)/(6*p+6) # ???
pp=p1*p2
df=p*(p+1)/2
qchisq(0.95,df) # 검정통계량 카이스퀘어 : 7.814
1-pchisq(pp,df) # 유의확률 0.05보다 작네
#여기까지 등분산 검정
(qd=qda(y~x1+x2))
predict(qd)
(tt=table(y,predict(qd)$class))
# cross validation
(qc=qda(y~x1+x2,CV=T))
new=data.frame(x1=c(20),x2=c(40))
predict(qd,new)
#plot
plot(x1,x2,pch=paste(y))
drawparti(y,x1,x2,method = "qda",col.wrong = "black",xlab = "x1",ylab = "y1")
t1x=trunc(qd$means[1,1]+7) # 판별의 중앙을 구하는듯
t1y=trunc(qd$means[1,2]+1) #
t2x=trunc(qd$means[2,1])
t2y=trunc(qd$means[2,2])
text(t1x,t1y,"mean of group1")
text(t2x,t2y,"mean of group2")

# 8. Mahalanobis Distance 비선형 분석 p.256 ####
x1=c(51,48,37,30,45,48,40, 22,11,25,15,23,30,19,30)
x2=c(45,31,28,27,50,40,37, 38,35,42,49,36,33,40,37)
y=c(2,2,2,2,2,2,2, 1,1,1,1,1,1,1,1)
xx1=cbind(x1[y==1],x2[y==1]) # 결과 1을 가지는 투입요소들
xx2=cbind(x1[y==2],x2[y==2]) # 결과 2를 가지는 투입요소들
s1=cov(xx1)
s2=cov(xx2)
n1=length(xx1); n2=length(xx2); p=2 # p는 뭐지? 결과 2개?
ss=((n1-1)*s1+(n2-1)*s2)/(n1+n2-2) #공분산수식
p1=(n1+n2-2)*log(abs(det(ss)))-(n1-1)*log(abs(det(s1)))-(n2-1)*log(abs(det(s2))) # ??
p2=1-(1/(n1-1)+1/(n2-1)-1/(n1+n2-2)) *  (2*p^2+3*p-1)/(6*p+6) # ???
pp=p1*p2
df=p*(p+1)/2
qchisq(0.95,df) # 검정통계량 카이스퀘어 : 7.814
1-pchisq(pp,df) # 유의확률 0.05보다 작네
# mahalanobis
d1=mahalanobis(xx,apply(xx1,2,mean),var(xx1))
d2=mahalanobis(xx,apply(xx2,2,mean),var(xx2))
(dd=round(cbind(d1,d2),3)) # 거리
(dr=round(prop.table(dd,1),3)) # dd의 거리 비율
dc=rep(0,length(y))
for(i in 1:length(y)){
  if(dr[i,1]<=dr[i,2]){
    dc[i]=1}
  else{
    dc[i]=2}
}
dc
table(y,dc)
# 중략 p.259

# 9. KNN p.260 ####
x1=c(50,48,35,31,45, 48,38,41,33,43, 22,11,25,25,23,
     33,25,29,30,32, 39,45,37,35,47, 28,29,30,31,19)
x2=c(35,31,38,35,42, 40,39,45,42,55, 38,35,42,49,36,
     35,38,37,42,30, 39,48,59,55,49, 39,45,40,49,50)
y=c(2,2,2,2,2, 1,1,1,1,1, 3,3,3,3,3, 2,2,2,2,2, 1,1,1,1,1, 3,3,3,3,3)
yy=factor(y)
ld=lda(y~x1+x2) # 선형판별
qd=qda(y~x1+x2) # 2차 판별
sk=sknn(yy~x1+x2,kn=3) # Knn 판별식
sk
par(mfrow=c(2,2))
drawparti(y,x1,x2,method = "lda",prec = 2000,xlab = "x1",ylab = "y1")
drawparti(y,x1,x2,method = "qda",prec = 2000,xlab = "x1",ylab = "y1")
drawparti(yy,x1,x2,method = "sknn",xlab = "x1",ylab = "y1")

#---------------------------------------------------------------------------
# 요인분석은 일반적으로 잠재적 성격을 보여준다, p.265 ####

# 10. factor analysis 요인분석 ####
x1=c(45,57,43,67,58, 39,57,35,58,57, 67,48,65,84,67, 90,35,57) # stablity
x2=c(56,68,69,79,55, 58,79,46,69,82, 84,52,74,68,48, 68,39,58) # healthy
x3=c(56,68,79,73,86, 54,58,79,47,69, 57,68,79,57,58, 57,58,58) # utility
x4=c(63,85,83,45,54, 56,67,47,79,80, 75,53,57,67,46, 58,84,52) # finess
x5=c(53,64,58,68,43, 48,63,55,70,88, 77,53,66,75,67, 87,56,46) # effectivity
xx=cbind(x1,x2,x3,x4,x5)
round(cor(xx),3)# 상관성이 있나 확인, 상관성 있음
eigen(cor(xx))$values #고유벡터 확인, 고유치 1.0을 넘는 것은 2개로서 요인 2개

(ff=factanal(xx,factors = 2,scores = c("regression")))
ff$scores # 요인 점수
ff$loadings #요인 부하량
# bar plot
par(mfrow=c(1,1))
cols=c("skyblue","yellow","lightgreen","lightgray","lightpink")
xx2=round(abs(ff$loadings),3)
barplot(xx2,main = "Bar plot: factor Loadings",col = cols)
# Positioning of variables
colnames(xx)=c("(안전성)","(보건성)","(편리성)","(쾌적성)","(효율성)")
plot(ff$loadings[,1:2],type = "n",xlim = c(-0.3,1.2))
text(ff$loadings[,1:2],colnames(xx))
grid()
title("factor analysis: Positioning of variables")
# factor score at observation
rownames(xx)=c(seq(1:18))
plot(ff$scores[,1:2],type = "n")
text(ff$scores[,1:2],rownames(xx))
grid()
title("factor analysis: factor score at observation")
# 비회전
fa=factanal(xx,factors = 2,scores = c("regression"),rotation = "none")
biplot(fa$scores,fa$loadings,ylabs = c("(안전성)","(보건성)","(편리성)","(쾌적성)","(효율성)"))
grid()
# 회전
ff=factanal(xx,factors = 2,scores = c("regression"))
biplot(ff$scores,ff$loadings,ylabs = c("(안전성)","(보건성)","(편리성)","(쾌적성)","(효율성)"))
grid()

#11. fa 요인분석 p.280 ####
library(psych)
f3t<-fa(Thurstone,3,n.obs = 213)
head(Thurstone,9) # 데이터확인
f3t
fa.diagram(f3t)
