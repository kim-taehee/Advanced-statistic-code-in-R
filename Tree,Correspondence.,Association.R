#---------------------------------------------------------------------------------
# tree model analysis 
# Correspondence
# Association
#---------------------------------------------------------------------------------
# tree model :  분기 기준(Split Criterion), 정지규칙(stopping Rule), 평가기준(eval. criterion)
# 기타생략 p.322, 뒤에 random forest 나옴

# 1.decision Tree p.326 ####
x1=c(21,24,35,37,42,46,54,56,62,66,69, 34,39,41,45,49,55,56,66,67,72,83)
x2=c(12,26,47,58,65,25,27,35,46,56,67, 26,35,43,56,68,27,34,46,57,65,69)
x3=c(15,55,34,63,26,66,38,68,39,74,67, 26,55,38,56,37,74,33,57,38,56,68)
y= c(1,1,1,1,1,1,1,1,1,1,1, 2,2,2,2,2,2,2,2,2,2,2)
xx=cbind(x1,x2,x3)
colnames(xx)=c("나이","경력","관심도")
require(rpart)
rt1=rpart(y~xx,method = "class") #메소드에 따라 추정값의 산정
print(rt1,digits = 2)
summary(rt1) # 트리별 분기 기준과 분류된 n의 수를 알 수 있다
require(psych)
plot(rt1,branch=0.6,margin = 0.05,main="Tree plot") # branch는 가지의 기울기, margin 외곽선
text(rt1,,use.n = T,all = T)
printcp(rt1) # prunning 가지치기 수행
plotcp(rt1) #cp와 비슷한 MIN+1SE 찾는 그림

# 2. Regression Tree p.331 ####
x1=c(22,34,35,36,28,35,46,42,39,25,36,25,38,40,37)
x2=c(25,16,23,33,31,25,39,41,30,19,20,21,30,41,42)
x3=c(51,11,24,21,26,52,56,55,26,59,55,21,18,19,22)
x4=c(10,11,21,22,13,15,24,10,16,14,35,16,12,14,10)
y=c(56,47,55,69,60,57,68,66,55,40,54,42,58,63,59)
rt2=rpart(y~x1+x2+x3+x4,method = "anova")
summary(rt2)
plot(rt2,branch=0.6,margin = 0.05,main="Tree plot") # branch는 가지의 기울기, margin 외곽선
text(rt2,,use.n = T,all = T)
# 현재 오류발생중

#----------------------------------------------------------------------------------
# 3. Correspondence analysis 대응분석 p.342 ####
xx=matrix(c(20,19,13,6,3,11, 15,13,7,12,14,8, 7,13,15,13,14,8,
            11,13,6,8,17,9,   8,14,6,13,12,5, 18,18,11,5,8,14,
            10,3,5,6,3,2, 2,5,3,6,7,8, 8,3,6,4,5,2), nr=6)
colnames(xx)=c("판매장접근성","판매장쾌적성","판매장인지성","주차장용의성",
               "시설편의성","종업원친절성","상품다양성","상품신선도","애프터서비스")
rownames(xx)=c("A매장","B매장","C매장","D매장","E매장","F매장")
margin.table(xx,margin = 1) # 가로합
margin.table(xx,margin = 2) # 세로합
library(MASS)
chisq.test(xx) #교차분석표의 독립성검정
(cc=corresp(xx,nf = min(nrow(xx),ncol(xx))-1) ) # 대응분석 수행, 축중에 작은값 -1의 크기를 요인수로함. 그래서5개
(c1=round(cc$cor,3)) # 정준계수
(c2=round(c1^2,3)) # 정준계수 ^2
(c3=round(c2/sum(c2),3)) # 기여율 (고유값/고유값합)
x=cc$rscore;y=cc$cscore # plot을 위한 x,y축 설정

# 4. MCA 다중대응분석 p.345 ####
x1=c("콜라","콜라","콜라","커피","커피"  ,"커피","커피","커피","커피","주스",
     "주스","주스","주스","주스","주스")
x2=c("중식","중식","양식","양식","양식"  ,"중식","중식","한식","한식","중식",
     "양식","한식","한식","한식","한식")
x3=c("과자","사탕","과자","과자","사탕"  ,"과자","사탕","과자","사탕","과일",
     "과자","과자","사탕","과일","과일")
dd=data.frame(cbind(x1,x2,x3))
(cc=mca(dd,3))
# plot
biplot(cc$rs,cc$cs,main = "MCA")
grid()
# 3d plot : 쓸일이 있을까?
x=cc$cs[,1]*cc$d[1]
y=cc$cs[,2]*cc$d[2]
z=cc$cs[,3]*cc$d[3]
library(scatterplot3d)
scatterplot3d(x,y,z,type="h",pch=c("콜라","커피","주스","중식","양식","한식",
                                   "과자","사탕","과일"))

# 5. 선호도자료의 대응분석 p.350 ####
a1=c(22,27,28,22,28,18,11,14,11,16,3,1,1,3,1)
a2=c(10,26,21,24,22,21,16,15,11,16,8,5,3,6,2)
a3=c(2,8,12,15,15,22,26,16,20,13,20,16,11,9,1)
a4=c(1,2,7,5,11,14,15,20,24,10,26,24,18,15,8)
aa=rbind(a1,a2,a3,a4)
colnames(aa)<-c("컵밥","삼각김밥","김밥","라면","피자","짜장면","짬뽕","볶음밥",
                "비빔밥","카레","김치찌개","된장찌개","생선구이","불고기","회덮밥")
rownames(aa)<-c("10대","20대","30대","40대")
chisq.test(aa)
bb=corresp(aa,nf=2)
plot(bb,cex=0.7,main="cor analysis plot")
grid()
abline(h=0,lty=c(4))
abline(v=0,lty=c(2))
legend("bottomright",legend=c("x축:간편형/정식형","y축:강한맛/약한맛"),cex=0.7,lty=c(4,2))

#-----------------------------------------------------------------------------------------
# 연관성 규칙은 리테일 산업에서 나온것으로, IBM의 Apriori 알고리즘을 현재 많이 사용한다
# 연관규칙의 평가기준 : event(X,Y)가 발생한경우 다이어그램으로 나타난다.
#  support 계수 :  전체에서 조건 X가 결론Y가 동시에 발생하는 비율로서, 연관성 상승
#  confidence 계수 : 조건X가 발생시, Y가 발생할 수 있는 비율,연관성 상승
#  Lift 계수 :  조건X와 결론Y가 각각 발생할 비율의 곱에 대한 동시발생의 비율, 1.0이상 good
#  support: |X ∩ Y| / |U|, Confi. : 조건부확률, Lift : Confi,/Lift

#  Apriori 알고리즘
#  신뢰도 지수로 상품을 추천하는데, 상품이 많을 경우 부하가 걸린다.
#  따라서 지지도 지수로 매출이 적은 상품을 걸러낸다.
#  그다음 신뢰도 지수로 matrix를 만들어 일정수준 이상이면 추천

library(arules)

# 6. 미국 조사자료 연관규칙 분석 p.405 ####

data(Income)
as(Income,"matrix")[1,] 
High.Income<-Income[Income %in% "income=$40,000+" ] # 전체 자료에서 고소득자 추출

itemFrequencyPlot(High.Income, main="Attiribute of \n High Income",
                   population=Income,support=0.2,col="light blue",lift=T,horiz=T) #plot
# 연관 규칙 구성
aa=apriori(Income)
Hi.Income=subset(aa,subset=rhs %in% "income=$40,000+" & lift>2) # 조건추출subset
inspect(head(sort(Hi.Income,by="support"),n=5)) # inspect가 조사인가? 다른건 ok
# 주요 특성(item) 조합의 도출 
es=eclat(Income)
summary(es) # 5571개 의 조합
Hi.Income2=subset(es,subset=items %in% "income=$40,000+" & size(items)>2)
inspect(head(sort(Hi.Income2,by="support"),n=5))
Hi.Income3=subset(es,subset=items %in% "income=$40,000+" & size(items)==2)
inspect(head(sort(Hi.Income3,by="support"),n=5))
# 고소득자의 집단 분류 
rich=subset(aa,subset=rhs %in% "income=$40,000+" & size(items)>3) # 
dd=dissimilarity(rich,method = "jaccard") # ???
#
c1=hclust(dd,"ward.D")$order[1:10]
inspect(rich[c1])

