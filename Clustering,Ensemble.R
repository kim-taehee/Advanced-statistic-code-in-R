#----------------------------------------------------------------------------------
# 인공신경망
# 군집분석
# 앙상블 
#----------------------------------------------------------------------------------
# 1.neural Network  (비선형 회귀분석) p.361 ####
xx=c(1,2,4,5,6,8,9,10,13,15,18,20,24,28,30) # 투입요소량 
yy=c(1,20,31,34,43,60,66,75,79,82,84,85,86,86,87) # 산출요소량
library(nnet)
ann=nnet(yy~xx,size=3,lineout=T)
1-var(residuals(ann))/var(yy)
plot(xx,yy)

# 2.neural Network  (비선형 판별분석) p.364 ####
x1=c(50,48,35,31,45, 48,38,41,33,42, 22,11,25,25,23,
     33,25,29,30,32, 39,45,37,35,47, 28,29,30,31,19)
x2=c(35,31,38,35,42, 40,39,45,42,55, 38,35,42,49,36,
     35,38,37,42,30, 39,48,59,55,49, 39,45,40,49,50)
y=factor(c(2,2,2,2,2   ,1,1,1,1,1    ,3,3,3,3,3,    2,2,2,2,2,  1,1,1,1,1,  3,3,3,3,3))
bnn=nnet(y~x1+x2,size=4,decay=0.1) # decay 지수 0.1
(cc=cbind(  predict(bnn,type = "raw"),  predict(bnn,type = "class")  ) )
new=data.frame(x1=33,x2=38)
predict(bnn,new)
# plot
xx1=rnorm(20000,mean = mean(x1),sd = sd(x1)) # x1에 대한 정규분포 생성
xx2=rnorm(20000,mean = mean(x2),sd = sd(x2)) # x2에 대한 정규분포 생성
new=data.frame(x1=xx1,x2=xx2)

pp=predict(bnn,new,type = "class"); pa=pc=rep(0,20000)
ypch=c(pa[pp==1]<-17,pa[pp==2]<-15,pa[pp==3]<-19) # point pch를 위한 list 생성
ycol=c(pc[pp==1]<-"red",pc[pp==2]<-"blue",pc[pp==3]<-"black")  # point col를 위한 list 생성
plot(xx1,xx2,pch=pa,col=pc,cex=0.5,main = "Discriminent Plot by \n NNT")

# 군집분석은 판별분석과 달리 y값의 범주가 명확하지 않을 때
# 계층적과 비계층적 군집방법이 있는데, 계층은 트리로하자
# 
# 3.  K-means cluster analysis p.295 ####
xx=matrix(c(88,72,94,68,92,85,77,88,71,85,71,83,73,63,79,90,80,70,87,
            74,74,88,65,77,68,63,73,80,63,62,60,62,74,67,74),7,5,byrow = TRUE)
colnames(xx)=c("안전성","보건성","편리성","쾌적성","효율성")
row.names(xx)=c("서울","부산","대구","대전","광주","인천","울산")
k1=kmeans(xx,centers = 2,algorithm = "Hartigan-Wong")
k2=kmeans(xx,centers = 3,algorithm = "Hartigan-Wong")
k3=kmeans(xx,centers = 4,algorithm = "Hartigan-Wong")
k1$betweenss/k1$totss  # 전체 변동에서 2개 군집의 비율
k2$betweenss/k2$totss  # 전체 변동에서 3개 군집의 비율
k3$betweenss/k3$totss  # 전체 변동에서 4개 군집의 비율
k3
# 분석사례
x<-rbind(matrix(rnorm(100,sd=0.3),ncol=2),
         matrix(rnorm(100,mean = 1,sd = 0.3),ncol = 2))
colnames(x)<-c("체중","신장")
head(x)
(Cl<-kmeans(x,5,nstart = 25)) # nstart: 한 집단의 최대군집수

#plot
library(cluster)
# plot(pam(x,3),ask=T) # ask는 어떤 그래프를 그릴 것인지 물어봄
# par(mfrow=c(1,1)) # 위에 것을 그리고 1:1비율로 초기화 해줘야 함
plot(x,col=Cl$cluster,pch=Cl$cluster)
points(Cl$centers,col=1:5,pch=16)
title("K-Means Plot \n 체중 - 신장 :5 groups")
legend("bottomright",c("1 Group","2 Group","3 Group","4 Group","5 Group"),
       col = 1:5,pch = 1:5,cex = 0.7)
# 외곽선 그리기 
library(rgeos)
p=x
for(i in 1:5){p=x[Cl$cluster==i,]
  pp=as(p[chull(p), ], "gpc.poly")  # chull은 (x,y)좌표값을 p로 바꾸어 폴리곤준비
  plot(pp,add=T)                    # as는gpc.poly class 로 변환
}

# 4. Item cluster anlaysis p.305 ####
library(psych)
data("bfi")
head(bfi) # row는 사람
ic<-iclust(bfi[1:25]) # clustering
title(" Item cluster analysis")
summary(ic)
# spidermap은 p.308 참조

# 5. 계량적 다차원 척도법의 분석 cmdscale  p.314 ####
x1=c(26,46,57,36,57, 26,58,37,36,56, 78,95,88,90,52, 56)
x2=c(35,74,73,73,62, 22,67,34,22,42, 65,88,90,85,46, 66)
x3=c(35,76,38,69,25, 25,87,79,36,26, 22,36,58,36,25, 44)
x4=c(45,89,54,55,33, 45,67,89,47,36, 40,56,68,45,37, 56)
xx2=cbind(x1,x2,x3,x4)
colnames(xx2)=c("국어","영어","수학","과학")
rownames(xx2)=seq(1:16)
(dd=dist(xx,method = "euclidean")) #유클리드적 상관 매트릭스
(cc=cmdscale(dd,k=2)) # 유클리드 거리로 변환된 자료를 다차원 척도법 새로운 좌표값 생
# 결과적으로 주성분 분석


# 6. SVM (비선형 판별분석) p.369 ####
x1=c(50,48,35,31,45, 48,38,41,33,42, 22,11,25,25,23,
     33,25,29,30,32, 39,45,37,35,47, 28,29,30,31,19)
x2=c(35,31,38,35,42, 40,39,45,42,55, 38,35,42,49,36,
     35,38,37,42,30, 39,48,59,55,49, 39,45,40,49,50)
y=factor(c(2,2,2,2,2   ,1,1,1,1,1    ,3,3,3,3,3,    2,2,2,2,2,  1,1,1,1,1,  3,3,3,3,3))
library(e1071)
sv=svm(y~x1+x2,type="C-classification")
# plot
xx1=rnorm(20000,mean = mean(x1),sd = sd(x1)) # x1에 대한 정규분포 생성
xx2=rnorm(20000,mean = mean(x2),sd = sd(x2)) # x2에 대한 정규분포 생성
new=data.frame(x1=xx1,x2=xx2)

pp=predict(sv,new,type = "class"); pa=pc=rep(0,20000)
ypch=c(pa[pp==1]<-17,pa[pp==2]<-15,pa[pp==3]<-19) # point pch를 위한 list 생성
ycol=c(pc[pp==1]<-"red",pc[pp==2]<-"green",pc[pp==3]<-"black")  # point col를 위한 list 생성
plot(xx1,xx2,pch=pa,col=pc,cex=0.5,main = "Discriminent Plot by \n NNT")
# nnt가 r^2:0.99로 훨씬 낫네

# 7. Self Organizing Map p.371 ####
x1=c(50,48,35,31,45, 48,38,41,33,42, 22,11,25,25,23,
     33,25,29,30,32, 39,45,37,35,47, 28,29,30,31,19)
x2=c(35,31,38,35,42, 40,39,45,42,55, 38,35,42,49,36,
     35,38,37,42,30, 39,48,59,55,49, 39,45,40,49,50)
y=factor(c(2,2,2,2,2   ,1,1,1,1,1    ,3,3,3,3,3,    2,2,2,2,2,  1,1,1,1,1,  3,3,3,3,3))
library(kohonen)
gr=somgrid(xdim=7,ydim=4,topo="hexagonal")
xx=data.frame(cbind(x1,x2))
ss=som(as.matrix(xx),gr,rlen=200) # rlen 학습횟수 ,gr은 솜그리드 
pp=predict(ss,trainX=as.matrix(xx),trainY=y)
# plot
plot(ss,type = "codes",main = "Plot by SOM")
plot(ss,type = "mapping",label=y,col=y,main = "Classify by SOM")

#---------------------------------------------------------------------------------
# 앙상블 기법은 최신의 것이 kaggle에 많다. 책은 구

# 8. bagging  p.380 ####
x1=c(50,48,35,31,45, 48,38,41,33,42, 22,11,25,25,23,
     33,25,29,30,32, 39,45,37,35,47, 28,29,30,31,19)
x2=c(35,31,38,35,42, 40,39,45,42,55, 38,35,42,49,36,
     35,38,37,42,30, 39,48,59,55,49, 39,45,40,49,50)
y=factor(c(2,2,2,2,2   ,1,1,1,1,1    ,3,3,3,3,3,    2,2,2,2,2,  1,1,1,1,1,  3,3,3,3,3))
library(adabag)
dd=data.frame(x1,x2,y)
bb=bagging(y~x1+x2,data=dd,mfinal=30,control=rpart.control(minsplit = 2)) # minsplit 최소 노드 수 ,cp = 복잡성계수

# 9. boosting p.384 ####
x1=c(50,48,35,31,45, 48,38,41,33,42, 22,11,25,25,23,
     33,25,29,30,32, 39,45,37,35,47, 28,29,30,31,19)
x2=c(35,31,38,35,42, 40,39,45,42,55, 38,35,42,49,36,
     35,38,37,42,30, 39,48,59,55,49, 39,45,40,49,50)
y=factor(c(2,2,2,2,2   ,1,1,1,1,1    ,3,3,3,3,3,    2,2,2,2,2,  1,1,1,1,1,  3,3,3,3,3))
library(adabag)
dd=data.frame(x1,x2,y)
ba=boosting(y~x1+x2,data=dd,mfinal=30,control=rpart.control(minsplit = 2)) # minsplit 최소 노드 수 ,cp = 복잡성계수

# 10. Random forest 
library(randomForest)
x1=c(50,48,35,31,45, 48,38,41,33,42, 22,11,25,25,23,
     33,25,29,30,32, 39,45,37,35,47, 28,29,30,31,19)
x2=c(35,31,38,35,42, 40,39,45,42,55, 38,35,42,49,36,
     35,38,37,42,30, 39,48,59,55,49, 39,45,40,49,50)
y=factor(c(2,2,2,2,2   ,1,1,1,1,1    ,3,3,3,3,3,    2,2,2,2,2,  1,1,1,1,1,  3,3,3,3,3))
dd=data.frame(x1,x2,y)
rf=randomForest(y~x1+x2,nodesize=2,mtry=1,ntree=100,importance=T,
                localImp=T,proximity=T,oob.prox=T) # mtry 분기적용 변수의 수,ntree tree수, nodesize 노드크기
plot(rf,main = "error rate")
