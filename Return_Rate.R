library(quantmod)
library(TTR)
library(xts)
library(plyr)
library(lattice)

load(file="C:\\RData\\result_BTCUSDT_5MIN_2017_8_16_2019_1_5.rda")
load(file="C:\\RData\\result_module_KD_manybuy_onesell_BTCUSDT_5MIN_2017_8_17_2018_12_15.rda")
load(file="C:\\RData\\result_module_MACD_manybuy_onesell_BTCUSDT_5MIN_2017_8_17_2018_12_15.rda")
load(file="C:\\RData\\result_module_MTM_manybuy_onesell_BTCUSDT_5MIN_2017_8_17_2018_12_15.rda")

mean_Cl_STK=c()
start_Cl_STK=c()
start_Date_STK=c()
for(k in 1:14){
  STK=result_STK[(88+(k-1)*10000):(10088+(k-1)*10000-1),]
  Cl_STK = STK[,"CL"]
  mean_Cl_STK=c(mean_Cl_STK,mean(Cl_STK))
  start_Cl_STK=c(start_Cl_STK,STK[1,"CL"])
  start_Date_STK=c(start_Date_STK,as.character(STK[1,"Date"]))
}
#head(start_Date_STK,14)
#head(mean_Cl_STK,14)

max11_KD=result_module_KD[,"max_earn11"]
max12_KD=result_module_KD[,"max_earn12"]
max13_KD=result_module_KD[,"max_earn13"]  
min11_KD=result_module_KD[,"min_earn11"]
min12_KD=result_module_KD[,"min_earn12"]
min13_KD=result_module_KD[,"min_earn13"]

max11_MACD=result_module_MACD[,"max_earn11"]
max12_MACD=result_module_MACD[,"max_earn12"]
max13_MACD=result_module_MACD[,"max_earn13"]  
min11_MACD=result_module_MACD[,"min_earn11"]
min12_MACD=result_module_MACD[,"min_earn12"]
min13_MACD=result_module_MACD[,"min_earn13"]

max11_MTM=result_module_MTM[,"max_earn11"]
max12_MTM=result_module_MTM[,"max_earn12"]
max13_MTM=result_module_MTM[,"max_earn13"]  
min11_MTM=result_module_MTM[,"min_earn11"]
min12_MTM=result_module_MTM[,"min_earn12"]
min13_MTM=result_module_MTM[,"min_earn13"]

Orig_data=data.frame(start_Date_STK,start_Cl_STK,mean_Cl_STK,
             max11_KD,max12_KD,max13_KD,
             max11_MACD,max12_MACD,max13_MACD,
             max11_MTM,max12_MTM,max13_MTM,
             min11_KD,min12_KD,min13_KD,
             min11_MACD,min12_MACD,min13_MACD,
             min11_MTM,min12_MTM,min13_MTM
             )

start_div_data=data.frame(start_Date_STK,start_Cl_STK,
                     max11_KD/start_Cl_STK,max12_KD/start_Cl_STK,max13_KD/start_Cl_STK,
                     max11_MACD/start_Cl_STK,max12_MACD/start_Cl_STK,max13_MACD/start_Cl_STK,
                     max11_MTM/start_Cl_STK,max12_MTM/start_Cl_STK,max13_MTM/start_Cl_STK,
                     min11_KD/start_Cl_STK,min12_KD/start_Cl_STK,min13_KD/start_Cl_STK,
                     min11_MACD/start_Cl_STK,min12_MACD/start_Cl_STK,min13_MACD/start_Cl_STK,
                     min11_MTM/start_Cl_STK,min12_MTM/start_Cl_STK,min13_MTM/start_Cl_STK
                     )

mean_div_data=data.frame(start_Date_STK,mean_Cl_STK,
                    max11_KD/mean_Cl_STK,max12_KD/mean_Cl_STK,max13_KD/mean_Cl_STK,
                    max11_MACD/mean_Cl_STK,max12_MACD/mean_Cl_STK,max13_MACD/mean_Cl_STK,
                    max11_MTM/mean_Cl_STK,max12_MTM/mean_Cl_STK,max13_MTM/mean_Cl_STK,
                    min11_KD/mean_Cl_STK,min12_KD/mean_Cl_STK,min13_KD/mean_Cl_STK,
                    min11_MACD/mean_Cl_STK,min12_MACD/mean_Cl_STK,min13_MACD/mean_Cl_STK,
                    min11_MTM/mean_Cl_STK,min12_MTM/mean_Cl_STK,min13_MTM/mean_Cl_STK
)

#KD 最大賺:~ 0.68 最大賠:~ -0.55 
xyplot(start_div_data$max11_KD.start_Cl_STK+
         start_div_data$max12_KD.start_Cl_STK+
         start_div_data$max13_KD.start_Cl_STK
         ~ start_div_data$start_Date_STK, start_div_data, type = "l" , col = c("red", "green", "blue"))
xyplot(start_div_data$min11_KD.start_Cl_STK+
         start_div_data$min12_KD.start_Cl_STK+
         start_div_data$min13_KD.start_Cl_STK
       ~ start_div_data$start_Date_STK, start_div_data, type = "l" , col = c("red", "green", "blue"))

#MACD 最大賺:~ 0.55 最大賠:~ -0.27
xyplot(start_div_data$max11_MACD.start_Cl_STK+
         start_div_data$max12_MACD.start_Cl_STK+
         start_div_data$max13_MACD.start_Cl_STK
       ~ start_div_data$start_Date_STK, start_div_data, type = "l" , col = c("red", "green", "blue"))
xyplot(start_div_data$min11_MACD.start_Cl_STK+
         start_div_data$min12_MACD.start_Cl_STK+
         start_div_data$min13_MACD.start_Cl_STK
       ~ start_div_data$start_Date_STK, start_div_data, type = "l" , col = c("red", "green", "blue"))

#MTM 最大賺:~ 0.68 最大賠:~ -0.53
xyplot(start_div_data$max11_MTM.start_Cl_STK+
         start_div_data$max12_MTM.start_Cl_STK+
         start_div_data$max13_MTM.start_Cl_STK
       ~ start_div_data$start_Date_STK, start_div_data, type = "l" , col = c("red", "green", "blue"))
xyplot(start_div_data$min11_MTM.start_Cl_STK+
         start_div_data$min12_MTM.start_Cl_STK+
         start_div_data$min13_MTM.start_Cl_STK
       ~ start_div_data$start_Date_STK, start_div_data, type = "l" , col = c("red", "green", "blue"))

#比較三個風險 KD MTM 風險高 報酬高 MACD 風險中 報酬中
xyplot(start_div_data$max11_KD.start_Cl_STK+
         start_div_data$max13_MACD.start_Cl_STK+
         start_div_data$max11_MTM.start_Cl_STK
       ~ start_div_data$start_Date_STK, start_div_data, type = "l" , col = c("red", "green", "blue"))
xyplot(start_div_data$min13_KD.start_Cl_STK+
         start_div_data$min11_MACD.start_Cl_STK+
         start_div_data$min13_MTM.start_Cl_STK
       ~ start_div_data$start_Date_STK, start_div_data, type = "l" , col = c("red", "green", "blue"))
