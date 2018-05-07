#----------------------------------------------------------------------------------
# 인공신경망
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

# 3. SVM (비선형 판별분석) p.369 ####
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

# 4. Self Organizing Map p.371 ####
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

# 6. bagging  p.380 ####
x1=c(50,48,35,31,45, 48,38,41,33,42, 22,11,25,25,23,
     33,25,29,30,32, 39,45,37,35,47, 28,29,30,31,19)
x2=c(35,31,38,35,42, 40,39,45,42,55, 38,35,42,49,36,
     35,38,37,42,30, 39,48,59,55,49, 39,45,40,49,50)
y=factor(c(2,2,2,2,2   ,1,1,1,1,1    ,3,3,3,3,3,    2,2,2,2,2,  1,1,1,1,1,  3,3,3,3,3))
library(adabag)
dd=data.frame(x1,x2,y)
bb=bagging(y~x1+x2,data=dd,mfinal=30,control=rpart.control(minsplit = 2)) # minsplit 최소 노드 수 ,cp = 복잡성계수

# 7. boosting p.384 ####
x1=c(50,48,35,31,45, 48,38,41,33,42, 22,11,25,25,23,
     33,25,29,30,32, 39,45,37,35,47, 28,29,30,31,19)
x2=c(35,31,38,35,42, 40,39,45,42,55, 38,35,42,49,36,
     35,38,37,42,30, 39,48,59,55,49, 39,45,40,49,50)
y=factor(c(2,2,2,2,2   ,1,1,1,1,1    ,3,3,3,3,3,    2,2,2,2,2,  1,1,1,1,1,  3,3,3,3,3))
library(adabag)
dd=data.frame(x1,x2,y)
ba=boosting(y~x1+x2,data=dd,mfinal=30,control=rpart.control(minsplit = 2)) # minsplit 최소 노드 수 ,cp = 복잡성계수

# 8. Random forest 
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
