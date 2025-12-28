install.packages("forecast")
install.packages("fpp2")

library(fpp2)
library(forecast)
install.packages("readxl")
library(readxl)
veri <- read_excel("C:\\Users\\elifs\\OneDrive\\Masaüstü\\zs_veri.xlsx", col_names = FALSE)
View(veri)
names(veri) <- paste0("Veri", 1:ncol(veri))
head(veri)
summary(veri)

veri_ts <- ts(veri, start = c(2010, 1),frequency=12)


#zaman serisi grafiğini çizme
ts.plot(veri_ts,gpars=list(xlab="Zaman", ylab="veri"))

#acf-pacf
Acf(veri_ts,lag.max = 42,  ylim=c(-1,1), lwd=3)
Pacf(veri_ts,lag.max = 42, ylim=c(-1,1), lwd=3)

#fark işlemleri
veri_ts1<-diff(veri_ts)
Acf(veri_ts1,lag.max = 42,  ylim=c(-1,1), lwd=3)

veri_m1<-diff(diff(veri_ts),12)
Acf(veri_m1,lag.max = 42,  ylim=c(-1,1), lwd=3)
Pacf(veri_m1,lag.max = 42, ylim=c(-1,1), lwd=3)

#seri durağan hale geldi

#toplamsal ayristirma
veri1_ts<-ma(veri_ts, order = 12, centre = TRUE)

Mevsim<- veri_ts-veri1_ts
donemort<-t(matrix(data=Mevsim, nrow=12))

colMeans(donemort, na.rm = T)  #her bir periyottaki donemlerin ortalamasi
sum(colMeans(donemort, na.rm = T))  #donem ortalamalari toplami
mean(colMeans(donemort, na.rm = T)) #donem ortalamalari ortalamasi

endeks<- colMeans(donemort, na.rm = T)-mean(colMeans(donemort, na.rm = T))
indeks<-  matrix(data = endeks, nrow = 180)
trenthata<- veri_ts-indeks

#seriyi hatadan arindirmak icin trenthata serisine dogrusal regresyon islemi uygulanir.
trent<-tslm(trenthata~trend)
#tahmin serisi (mevsimsel endeks+saf trent serisi)
tahmin<- indeks+trent[["fitted.values"]]

#hata serisi
hata<- veri_ts-indeks-trent[["fitted.values"]]

plot( window(veri_ts), 
      xlab="Zaman", ylab="",lty=1, col=4, lwd=2, ylim=c(19,320))
lines( window(tahmin) ,lty=3,col=2,lwd=3)
legend("topleft",c(expression(paste(veri )),
                   expression(paste(Tahmin ))),
       lwd=c(2,2),lty=c(1,3), cex=0.6, col=c(4,2))

#hatalar akgurultu mu?
Acf(hata,main="Hata", lag.max = 42,  ylim=c(-1,1), lwd=3)
Pacf(hata,main="Hata",lag.max = 42, ylim=c(-1,1), lwd=3)

Box.test (hata, lag = 42, type = "Ljung")
# hatalar akgürültü olmadığı için toplamsal ayrıştırma yöntemi kullanılmaz

#çarpımsal ayrıştırma yöntemi

Mevsim1 <- veri_ts/veri1_ts

#her bir periyot icin ortalama degerlerinin hesabi
donemort1<-t(matrix(data=Mevsim1, nrow = 12))

colMeans(donemort1, na.rm = T)

#toplam
sum(colMeans(donemort1, na.rm = T))

#ortalamalarin ortalamasi
mean(colMeans(donemort1, na.rm = T))

#mevsimsel endeks degerleri
endeks1<- colMeans(donemort1, na.rm = T)/mean(colMeans(donemort1, na.rm = T))

#endeks degerlerini seri boyunca yazdirma islemi
indeks1<-  matrix(data = endeks1, nrow = 180)


#trent serisi 
trenthata1<- veri_ts/indeks1

#hatadan arindirma islemi
trent1<- tslm(trenthata1~trend)
tahmin1<- indeks1*trent1[["fitted.values"]] #tahmin=endeks*orijinal serinin trent bileseni

#hata serisi
hata1<- veri_ts-tahmin1


#orijinal seri ile tahmin serisinin uyumu
plot( window(veri_ts), 
      xlab="Zaman", ylab="",lty=1, col=4, lwd=2)
lines( window(tahmin1) ,lty=3,col=2,lwd=3)
legend("topleft",c(expression(paste(veri )),
                   expression(paste(Tahmin ))),
       lwd=c(2,2),lty=c(1,3), cex=0.6, col=c(4,2))

#hatalar akgurultu mu?
Acf(hata1,main="Hata", lag.max = 42,  ylim=c(-1,1), lwd=3)
Pacf(hata1,main="Hata",lag.max = 42, ylim=c(-1,1), lwd=3)


Box.test (hata1, lag = 24, type = "Ljung")

#Çarpımsal ayrıştırma yöntemi de hatalar akgrültü değil yani kullanılamaz


##regresyon

#toplamsal model

t<-1: 1: 180

cos1<-cos(2*3.1416*t/12)

veriseti<-as.data.frame(cbind(veri, t, cos1))

names(veriseti)<- c("y", "t", "cos1")
attach(veriseti)

regresyon.model1<-lm(y~t+cos1)
summary(regresyon.model1)


install.packages("lmtest")
library(lmtest)

##durbin-watson testi
dwtest(y~t+cos1)

tahmin1<-predict(regresyon.model1)
sinir1<-predict(regresyon.model1, interval = 'confidence' ,level = .95)
hata1<-resid(regresyon.model1)

plot( window(y),
      xlab="", ylab="", type="l", lty=3, col=2, lwd=2)
lines(window(sinir1[,2]) ,type="l",lty=1,col=4,lwd=2)
lines(window(sinir1[,3]) ,type="l",lty=1,col=3,lwd=2)
legend("topleft",c(expression(paste(veri)),
                   expression(paste(Altsinir)),
                   expression(paste(ustsinir))),
       lwd=c(2,2,2),lty=c(3,1,1), cex=0.7, col=c(2,4,3))


#Hatalar akgurultu mu?
Acf(hata1,lag.max = 42,  ylim=c(-1,1), lwd=3)

#Box-Ljung
Box.test(hata1, lag = 24, type = "Ljung")

##Carpimsal model

c1<-t*cos(2*3.1416*t/12)

veriseti3<-as.data.frame(cbind(veri, t, c1))

names(veriseti3)<- c("y", "t", "c1")
attach(veriseti3)

regresyon.model3<-lm(y~t+c1)
summary(regresyon.model3)

##durbin-watson testi
dwtest(y~t+c1)

tahmin3<-predict(regresyon.model3)
sinir3<-predict(regresyon.model3, interval = 'confidence' ,level = .95)
hata3<-resid(regresyon.model3)


plot( window(y),
      xlab="", ylab="", type="l", lty=3, col=2, lwd=2)
lines(window(sinir3[,2]) ,type="l",lty=1,col=4,lwd=2)
lines(window(sinir3[,3]) ,type="l",lty=1,col=3,lwd=2)
legend("topleft",c(expression(paste(veri)),
                   expression(paste(Altsinir)),
                   expression(paste(ustsinir))),
       lwd=c(2,2,2),lty=c(3,1,1), cex=0.7, col=c(2,4,3))

#Hatalar akgurultu mu?
Acf(hata3,lag.max = 42,  ylim=c(-1,1), lwd=3)

#Box-Ljung
Box.test(hata3, lag = 24, type = "Ljung")

#üstel düzleştirme winters

Winters1<- ets(veri_ts, model = "AAA")
summary(Winters1)
Winters2<- ets(abs(veri_ts), model = "MAM")
summary(Winters2)


e <- residuals(Winters1)

#Hataların ACF grafiği
acf(e, main = "Winters (ETS AAA) Artıklar - ACF")
forecast::checkresiduals(Winters1)

mam_fit <- ets(veri_ts, model = "MAM", damped = FALSE)
summary(mam_fit)
forecast::checkresiduals(mam_fit)

e2 <- residuals(Winters2)
acf(e2, main = "Winters (ETS MAM) Artıklar - ACF")


## mevsimsel box jenkins

veri_arima1 <- Arima(veri_ts, order = c(3,1,0), seasonal= c(1,1,0), include.constant=TRUE)
coeftest(veri_arima1)##anlamlı   854.33
summary(veri_arima1)

veri_arima2 <- Arima(veri_ts, order = c(3,1,0), seasonal= c(2,1,0), include.constant=TRUE)
coeftest(veri_arima2)##anlamsız
summary(veri_arima2)

veri_arima3 <- Arima(veri_ts, order = c(3,1,0), seasonal= c(0,1,1), include.constant=TRUE)
coeftest(veri_arima3)##anlamsız
summary(veri_arima3)

veri_arima4 <- Arima(veri_ts, order = c(3,1,0), seasonal= c(0,1,2), include.constant=TRUE)
coeftest(veri_arima4)##anlamsız
summary(veri_arima4)

veri_arima5 <- Arima(veri_ts, order = c(3,1,0), seasonal= c(1,1,1), include.constant=TRUE)
coeftest(veri_arima5) ##anlamsız
summary(veri_arima5)

veri_arima6 <- Arima(veri_ts, order = c(3,1,0), seasonal= c(0,1,0), include.constant=TRUE)
coeftest(veri_arima6)##anlamlı 866.68
summary(veri_arima6)

veri_arima7 <- Arima(veri_ts, order = c(0,1,1), seasonal= c(0,1,1), include.constant=TRUE)
coeftest(veri_arima7) ##anlamlı 838.49
summary(veri_arima7)

veri_arima8 <- Arima(veri_ts, order = c(0,1,1), seasonal= c(0,1,2), include.constant=TRUE)
coeftest(veri_arima8) ##anlamsız
summary(veri_arima8)

veri_arima9 <- Arima(veri_ts, order = c(0,1,1), seasonal= c(1,1,0), include.constant=TRUE)
coeftest(veri_arima9)## anlamlı 846,12
summary(veri_arima9)

veri_arima10 <- Arima(veri_ts, order = c(0,1,1), seasonal= c(2,1,0), include.constant=TRUE)
coeftest(veri_arima10)## anlamsız
summary(veri_arima10)

veri_arima11 <- Arima(veri_ts, order = c(0,1,1), seasonal= c(1,1,1), include.constant=TRUE)
coeftest(veri_arima11)## anlamlı 839,34
summary(veri_arima11)

veri_arima12 <- Arima(veri_ts, order = c(0,1,1), seasonal= c(2,1,1), include.constant=TRUE)
coeftest(veri_arima12)## anlamsız
summary(veri_arima12)

veri_arima13 <- Arima(veri_ts, order = c(0,1,1), seasonal= c(1,1,2), include.constant=TRUE)
coeftest(veri_arima13)## anlamsız
summary(veri_arima13)

veri_arima14 <- Arima(veri_ts, order = c(0,1,1), seasonal= c(0,1,0), include.constant=TRUE)
coeftest(veri_arima14)## anlamlı 861.18
summary(veri_arima14)

veri_arima15 <- Arima(veri_ts, order = c(3,1,1), seasonal= c(0,1,0), include.constant=TRUE)
coeftest(veri_arima15)## anlamsız
summary(veri_arima15)

veri_arima16 <- Arima(veri_ts, order = c(3,1,1), seasonal= c(1,1,0), include.constant=TRUE)
coeftest(veri_arima16)## anlamsız
summary(veri_arima16)

veri_arima17 <- Arima(veri_ts, order = c(3,1,1), seasonal= c(0,1,1), include.constant=TRUE)
coeftest(veri_arima17)## anlamsız
summary(veri_arima17)

veri_arima18 <- Arima(veri_ts, order = c(2,1,1), seasonal= c(0,1,0), include.constant=TRUE)
coeftest(veri_arima18)## anlamsız
summary(veri_arima18)

veri_arima19 <- Arima(veri_ts, order = c(2,1,1), seasonal= c(1,1,0), include.constant=TRUE)
coeftest(veri_arima19)## anlamsız
summary(veri_arima19)

veri_arima20 <- Arima(veri_ts, order = c(2,1,1), seasonal= c(0,1,1), include.constant=TRUE)
coeftest(veri_arima20)## anlamsız
summary(veri_arima20)

veri_arima21 <- Arima(veri_ts, order = c(1,1,1), seasonal= c(0,1,0), include.constant=TRUE)
coeftest(veri_arima21)## anlamsız
summary(veri_arima21)

veri_arima22 <- Arima(veri_ts, order = c(1,1,1), seasonal= c(0,1,1), include.constant=TRUE)
coeftest(veri_arima22)## anlamsız
summary(veri_arima22)

veri_arima23 <- Arima(veri_ts, order = c(1,1,1), seasonal= c(1,1,0), include.constant=TRUE)
coeftest(veri_arima23)## anlamsız
summary(veri_arima23)

veri_arima24 <- Arima(veri_ts, order = c(1,1,1), seasonal= c(1,1,1), include.constant=TRUE)
coeftest(veri_arima24)## anlamsız
summary(veri_arima24)

veri_arima25 <- Arima(veri_ts, order = c(1,1,0), seasonal= c(0,1,0), include.constant=TRUE)
coeftest(veri_arima25)## anlamlı 869,97
summary(veri_arima25)

veri_arima26 <- Arima(veri_ts, order = c(1,1,0), seasonal= c(1,1,0), include.constant=TRUE)
coeftest(veri_arima26)## anlamlı 854,91
summary(veri_arima26)

veri_arima27 <- Arima(veri_ts, order = c(1,1,0), seasonal= c(0,1,1), include.constant=TRUE)
coeftest(veri_arima27)## anlamlı 847.68
summary(veri_arima27)

veri_arima28 <- Arima(veri_ts, order = c(1,1,0), seasonal= c(1,1,1), include.constant=TRUE)
coeftest(veri_arima28)## anlamlı 847.92
summary(veri_arima28)

veri_arima29<- Arima(veri_ts, order = c(1,1,0), seasonal= c(2,1,0), include.constant=TRUE)
coeftest(veri_arima29)## anlamsız
summary(veri_arima29)

veri_arima30 <- Arima(veri_ts, order = c(1,1,0), seasonal= c(2,1,1), include.constant=TRUE)
coeftest(veri_arima30)## anlamsız
summary(veri_arima30)

veri_arima31 <- Arima(veri_ts, order = c(1,1,0), seasonal= c(0,1,2), include.constant=TRUE)
coeftest(veri_arima31)## anlamsız
summary(veri_arima31)

veri_arima32 <- Arima(veri_ts, order = c(1,1,0), seasonal= c(1,1,2), include.constant=TRUE)
coeftest(veri_arima32)## anlamsız
summary(veri_arima32)

veri_arima33<- Arima(veri_ts, order = c(1,1,0), seasonal= c(2,1,2), include.constant=TRUE)
coeftest(veri_arima33)## anlamsız
summary(veri_arima33)

veri_arima34 <- Arima(veri_ts, order = c(3,1,1), seasonal= c(1,1,1), include.constant=TRUE)
coeftest(veri_arima34)## anlamsız
summary(veri_arima34)


hata1<- veri_arima1[["residuals"]]
Box.test (hata1, lag = 24, type = "Ljung")

hata2<- veri_arima6[["residuals"]]
Box.test (hata2, lag = 24, type = "Ljung")

hata3<- veri_arima7[["residuals"]]
Box.test (hata3, lag = 24, type = "Ljung")

hata4<- veri_arima9[["residuals"]]
Box.test (hata4, lag = 24, type = "Ljung")

hata5<- veri_arima11[["residuals"]]
Box.test (hata5, lag = 24, type = "Ljung")

hata6<- veri_arima14[["residuals"]]
Box.test (hata6, lag = 24, type = "Ljung")

hata7<- veri_arima25[["residuals"]]
Box.test (hata7, lag = 24, type = "Ljung")

hata8<- veri_arima26[["residuals"]]
Box.test (hata8, lag = 24, type = "Ljung")

hata9<- veri_arima27[["residuals"]]
Box.test (hata9, lag = 24, type = "Ljung")

hata10<- veri_arima28[["residuals"]]
Box.test (hata10, lag = 24, type = "Ljung")

#ongoru

ongoru<- forecast(veri_arima7 , h=5)

ongoru[["mean"]]












