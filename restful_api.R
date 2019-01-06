library(jsonlite)
#install.packages("C:/RData/RCurl_1.95-4.11.zip", repos=NULL, type="source")
#安裝檔在https://cran.r-project.org/web/packages/RCurl/index.html
#介紹:https://yijutseng.github.io/DataScienceRBook/io.html#api
library(RCurl)
library(quantmod)
library(TTR)
library(xts)
#設定資料取得
options(scipen = 999) #不要科學記號
year_ = 2017 #BTC 2017 ETH 2017 LTC 2017
month_ = 12 #BTC 8 ETH 8 LTC 12
day_ = 12 #BTC 16 ETH 16 LTC 12
total_data = c()
for( i in 1:390){ #2017/8/16-2019/1/5 est 508 #2017/12/12-2019/1/5 est 390
  print(i)
  url = "https://api.binance.com/api/v1/klines?"
  symbol = "symbol=LTCUSDT&" #BTC ETH LTC XRP XMR
  interval = "interval=5m&" #interval:SECOND,MINUTE,DAY #12*24=288 4*24=96 1*24=24  6
  limit = "limit=288&startTime=" #limit:default:500 MAX:1000 #one day
  year = as.character(year_)
  month = as.character(month_)
  day = as.character(day_)
  temp=paste(year,month,day,sep = "-")
  #BTC 23:00:00 ETH 23:00:00 LTC 22:30:00
  startTime = toString(as.numeric(as.POSIXct(paste(temp,"22:30:00 EST",sep = " ")))*1000) 
  #startTime:EST 東岸時間
  
  restful_api = paste(url,symbol,interval,limit,startTime,sep="")
  JSONData<-fromJSON(getURL(restful_api))
  print(restful_api)
  
  Data=as.matrix(JSONData,keep.rownames = TRUE)
  Date=as.character(as.POSIXct(as.numeric(as.character(Data[,1]))/1000,origin="1970-01-01",tz="EST"))
  Data_Date=cbind(Data,Date)
  colnames(Data_Date) <- c("Opentime","Open","High","Low","Close","Volume","Closetime","Quoteassetvolume","Numberoftrades","Takerbuybaseassetvolume","Takerbuyquoteassetvolume","Ignore","Date")
  total_data=rbind(total_data,Data_Date)
  day_=day_+1
  if(day_==31 && (month_==4 || month_==6 || month_==9 || month_==11)){
    day_=1
    month_=month_+1
  }else if(day_==32 && (month_==1 || month_==3 || month_==5 || month_==7 || month_==8 || month_==10 || month_==12)){
    day_=1
    month_=month_+1
    if(month_==13){
      month_=1
      year_=year_+1
    }
  }else if(day_==29 && month_==2 && (year_%%4 != 0)){
    day_=1
    month_=month_+1
  }else if(day_==30 && month_==2 && (year_%%4 == 0)){
    day_=1
    month_=month_+1
  }
}
head(total_data)
tail(total_data)
save(total_data, file="C:\\RData\\LTCUSDT_5MIN_2017_12_12_2019_1_5.rda")