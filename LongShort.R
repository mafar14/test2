
getwd()
setwd("/Users/Pedra/Desktop/Modelos")

#load package
library(quantmod)
library(PairTrading)

#import ibovespa vector
serie_de_dados<-read.table(file = "/Users/Pedra/Desktop/Modelos/ibovespa.csv",header = TRUE)
ibovespa<-array(serie_de_dados$Ticker)

#baixando os preços
stock.price<-mget(getSymbols(ibovespa, from = "2019-06-01",src = "yahoo"))

#montando price_matrix
N<-nrow(ibovespa)
price_matrix<-array(data = 0)
for(i in 1:N){
  preco_fechamento<-stock.price[[i]][,4]
  price_matrix<-cbind(price_matrix,preco_fechamento)
}
#retira a coluna 1
price_matrix<-price_matrix[,-1]

pairs_trading<-array(data = 0,dim = c(10000,4))
colnames(pairs_trading)<-c("pair1","pair2","Estacionario","Correlacao")

#load stock price data
for(i in 1:N){
  t<-i+1
  for(j in t:N){
    pair<-cbind(price_matrix[,i],price_matrix[,j])
    pairs_trading[j,1]<-colnames(pair)[1]
    pairs_trading[j,2]<-colnames(pair)[2]
    #removing NAs
    linhas<-which(is.na(pair))
    pair<-pair[-linhas]
    #Estimando parametros
    param<-EstimateParameters(price.pair = pair,method = lm)
    #removing NAs
    NAs_spread<-which(is.na(param$spread))
    if(is.null(NAs_spread)){
      spread<-param$spread
    } else {
      spread<-param$spread[-NAs_spread,]
    }  
    pairs_trading[j,3]<-IsStationary(spread, 0.1)[[2]]
    pairs_trading[j,4]<-cor(pair)[1,2]
  }
}

#Estimate parameters & plot spread
spread<-EstimateParameters(price.pair = stock.price,method = lm)
str(spread)
plot(spread$spread)
IsStationary(spread$spread, 0.1)

#estimate parameters for back test
params <- EstimateParametersHistorically(price.pair = stock.price, period = 180)
#create & plot trading signals
str(params)

#create & plot trading signals
signal <-Simple(params$spread, 0.05)
barplot(signal,col="blue",space = 0, border = "blue",xaxt="n",yaxt="n",xlab="",ylab="")
par(new=TRUE)
plot(params$spread)


write.table(x = stock.price,file = "/Users/Pedra/Desktop/Modelos/stock.price.txt")



