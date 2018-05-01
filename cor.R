#####################################################################
# simple correlation analysis
# partial correlation analysis
# covariation analysis
#####################################################################

# 1.two cor-test  p.73 ####
# T=r*sqrt{(n-2)/(1-r^2)}
# make hypothesis, check siginificant level, decise test statistic
x1=c(48,45,51,49,50,51,52,45,48,49) # data1
x2=c(55,56,52,53,50,45,49,52,50,51) # data2
cov(x1,x2) #cov
cor.test(x1,x2)

# 2. two cor-test(standardization)  p.74 ####
# T = 1/2  * log {(1+r)/(1-r)},  z=sqrt(n-3)*[(t-1/2)*log{(1+p)/(1-p)}]
# p는 귀무가설 상관관계
x1=c(48,45,51,49,50,51,52,45,48,49) # data1
x2=c(55,56,52,53,50,45,49,52,50,51) # data2
r=-0.4  # p
rr=cor(x1,x2)
n=length(x1)
t=0.5*log((1+rr)/(1-rr)) # t 산정
z=sqrt(n-3)*(t-0.5*log((1+r)/(1-r)))
2*(1-pnorm(abs(z)))

# 3. N cor-test p.75 ####
#3개 변량에 대해서 cor-box 그리기 
x3=c(53,55,56,61,63,55,53,55,51,NA)
xx=matrix(c(x1,x2,x3),nc=3)

panel.hist<-function(x,...){
  usr<-par("usr"); on.exit(par(usr))
  par(usr=c(usr[1:2],0,1.5))
  h<-hist(x, plot=FALSE)
  breaks <- h$breaks; nB <-length(breaks)
  y<- h$counts; y <- y/max(y)
  rect(breaks[-nB],0,breaks[-1],y,col = "gray",...)
}


pairs(xx,labels = c("xx1","xx2","xx3"),
      main="3 variable pair plot for Cor-Analysis",
      cex=1.5,pch=24,bg="light blue",
      diag.panel = panel.hist, cex.labels = 1.5,font.labels = 1.5,
      upper.panel = NULL)

r12=signif(cor.test(x1,x2)$estimate,digits=4)
r13=signif(cor.test(x1,x3)$estimate,digits=4)
r23=signif(cor.test(x2,x3)$estimate,digits=4)
text(0.5,0.8,paste("r12=",r12))
text(0.8,0.8,paste("r13=",r13))
text(0.8,0.5,paste("r23=",r23))

# 4. N variable partial cor-test  p.78 ####
x1=c(48,45,51,49,50,51,52,45,48,49) # data1
x2=c(55,56,52,53,50,45,49,52,50,51) # data2
x3=c(53,55,56,61,63,55,53,55,51,NA) # data3
xx=matrix(c(x1,x2,x3),nc=3) # matrix
cov(xx,use = "na.or.complete") #cov
cor(xx,use = "na.or.complete") #cor
cor.test(x1,x2)$p.value #cor p-value
library(corpcor)
cor2pcor(cor(xx,use = "na.or.complete"))

# 5. 2 variable Rank cor-test  p.80 ####
# 제품간 선호도가 종속인가 
#  3개 이상일 때는 matrix를 만든다 
y1=c(1,1,2,3,3,5,5,5,7,7,7,8,8,8,9,9)
y2=c(3,5,6,3,9,8,1,2,3,1,3,4,6,3,1,2)
cor.test(y1,y2)
cor.test(y1,y2,method = "kendall",exact = F) #비모수,명목 
cor.test(y1,y2,method = "spearman",exact = F) #랭크

# 6. Ancova  p.85 (이것으로 지역효과 월효과 확인 가능) ####
cc=c(65,56,77,57,48 ,55,35,41,39,38 ,88,64,94,74,69, 66,46,76,56,44 ,55)
xx=gl(2,10,21,labels = c("남자","여자"))
yy=gl(3,7,21,labels = c("동","서","남"))
summary(lm(cc~xx))


# library(ggally)에서 ggpair 그리면 됨 