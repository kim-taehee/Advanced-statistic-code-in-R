kings<-scan("http://robjhyndman.com/tsdldata/misc/kings/dat",skip = 3)
kings
kings.ts<-ts(kings)


births<-scan("http://robjhyndman.com/tsdldata/data/nybirth.data")
births.ts<-ts(births,frequency = 12,start = c(1946,1))


souv<-scan("http://robjhyndman.com/tsdldata/data/fancy.dat")
souv.ts<-ts(souv,frequency = 12,start = c(1987,1))

#plotting time series
plot.ts(kings.ts)  # which varidation? random
plot.ts(births.ts)   # random and seasonal
plot.ts(souv.ts)    #  random and seasonal but not addictive
logsouv.ts<-l0g(souv.ts)


###### 자료를 요소별로 분해하기 ######
library(TTR)
kings.ts.sma3<-SMA(kings.ts,n=3)
plot.ts(kings.ts.sma3) #최초 plot과 비교
kings.ts.sma8<-SMA(kings.ts,n=8)
plot.ts(kings.ts.sma8) # 단순이동평균으로 불규칙 변동을 제거하고 트렌드를 살펴봄
# 20번째 왕까지는 38세에서 55까지 수명유지하고 그 이후부터는 수명이 늘어서 40번째 왕은 73세
library(forecast)
forecast(kings.ts.sma8,h=3)
plot(forecast(kings.ts.sma8,h=3))

# decomposing seasonal data(trend + seasonal + irragular)
births.ts.comp<-decompose(births.ts)
plot(births.ts.comp)

# seasonally adjusting
births.ts.comp.adj <- births.ts-birth.ts.comp$seasonal
plot(forecast(births.ts.comp.adj,h=12))





######  단수지수 평활 #######
# simple exponential smoothing (without trend and seasonality)
# 0 =< a =< 1 (0에 가까우면 과거자료와 최근자료를 가중치 함)

rain <- sacn("http://robjhyndman.com/tsdldata/hurst/precip1.dat",skip=1)
# 런던 연 강수량
rain.ts<-ts(rain,start = c(1813))
plot(rain.ts)
rain.ts.ses<-HoltWinters(rain.ts,beta = F,gamma = F)
rain.ts.ses$fitted
plot(rain.ts.ses)  # red line이 예측
rain.ts.ses$SSE   # 오차 제곱합 구하기
rain.ts.forecast<-forecast.HoltWinters(rain.ts.ses, h=8)
plot.forecast(rain.ts.forecast)


###### 모형 적합도 확인  ######
# 모형적합도 확인 : 오차가 자기상관이 미미하고, 정규분포와 등분산을 보인다면 해당 모델 굿
acf(rain.ts.forecast$residuals, log.max=20)
Box.test(rain.ts.forecast$residuals,lag = 20,type = "Ljung-Box")
# 귀무가설 자기 상관이 없다.
library(car)
library(caret)
rain.ts.forecast$residuals2<-as.vector(rain.ts.forecast$residuals)
durbinWatsonTest(rain.ts.forecast$residuals2)  #  1.5<&<2.5이면 자기상관이 없다.
plot.ts(rain.ts.forecast$residuals) # 등분산 확인




####### 표준 정규분포 그래프 그리기 ######
plotForecastErrors<-function(forecastErrors){
  
  mybinsize <-IQR(forecastErrors)/4
  mysd<-sd(forecastErrors)
  mymin<-min(forecastErrors) - mysd*5
  mymax<-max(forecastErrors) - mysd*3
  # generate normally distributed data with mean 0 and standard deviation mysd
  mynorm <- rnorm(10000, mean = 0,sd=mysd)
  mymin2<-min(mynorm)
  mymax2<-max(mynorm)
  
  if(mymin2 < mymin){mymin<-mymin2}
  if(mymax2 > mymax){mymax<-mymax2}
  # make a red histogram of the forecast errors, with the normally distributed data
  mybins <- seq(mymin,mymax,mybinsize)
  hist(forecastErrors,col = "red",freq = FALSE,breaks = mybins)
  # freq=FALSE ensures the area under the histogram =1
  # generate normally distributed data with mean 0 and standard deviation mysd
  myhist <-hist(mynorm,plot = FALSE , breaks = mybins)
  # plot the normal curve as a blue line on the top of histogram of forecastErroes :
  points(myhist$mids,myhist$density,type="l",col="blue",lwd=2)
}
plotForecastErrors(rain.ts.forecast$residuals)




####### 홀트 지수 평활법 #######
skirts<-scan("http://robjhyndman.com/tsdldata/roberts/skirts.dat",skip = 5)
skirts.ts<-ts(skirts,start = c(1866))
plot.ts(skirts.ts)
skirts.ts.forecast <- HoltWinters(skirts.ts,gamma = F)
skirts.ts.forecast$SSE
plot(skirts.ts.forecast)
HoltWinters(skirts.ts, gamma = FALSE, l.start = 608 , b.start = 9)
#level 과  slope를 다르게 할 때는 레벨은 최초 관측값으로, 기울기는 두번째-첫째
skirts.ts.forecast2<-forecast.HoltWinters(skirts.ts.forecast,h=19)
plot.forecast(skirts.ts.forecast2)
acf(skirts.ts.forecast2$residuals,lag.max = 20)  # 한 개 있으나 영향이 적을듯
Box.test(skirts.ts.forecast2$residuals,lag = 20,type = "Ljung-Box") # 방법1
skirts.ts.forecast2$residuals2<-as.vector(skirts.ts.forecast2$residauls)
durbinWatsonTest(skirts.ts.forecast2$residuals2) # 방법2
plot.ts(skirts.ts.forecast2$residuals)  # 예측오차가 등분산인지 확인
plotForecastErrors(skirts.ts.forecast2$residuals) # 예측오차가 정규분포인지 확인

###### arima 모델 ########
king.diff<-diff(kings.ts,differences = 1)
plot.ts(kings.diff)


acf(king.diff,lag.max = 20)  #plot a correlogram, ARMA(0,1)
acf(king.diff,lag.max = 20,plot = F) # get the autocorrelation values
pacf(king.diff,lag.max = 20) #plot a partial correlogram, ARMA(3,0)
pacf(king.diff,lag.max = 20,plot = F) # get the partial autocorrelation values
# ARMA(0,1)이 간결하므로 선택하여 최종모델은 ARIMA(0,1,1)
auto.arima(kings)  # 자동으로 아리마분석
kings.ts.arima <-arima(kings.ts,order = c(0,1,1))
kings.ts.arima
kings.ts.fore<-forecast.Arima(kings.ts.arima,h=5)
plot.forecast(kings.ts.fore)
# 4 model 검증 
acf(kings.ts.fore$residuals,lag.max = 20)
Box.test(kings.ts.fore$residuals,lag = 20,type = "Ljung-Box")
plot.ts(kings.ts.fore$residuals)
plotForecastErrors(kings.ts.fore$residuals)




