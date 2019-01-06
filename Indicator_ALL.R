library(quantmod)
library(TTR)
library(xts)

load(file="C:\\RData\\ETHUSDT_4HR_2017_8_16_2019_1_5.rda")
head(total_data)
STK=total_data
Cl_STK = as.numeric(STK[,5])
Op_STK = as.numeric(STK[,2])
High_STK = as.numeric(STK[,3])
Low_STK = as.numeric(STK[,4])
Vol_STK = as.numeric(STK[,6])
Date_STK = STK[,13]

#KD運算
#設定
KD_N = 9

Rsv_STK = c(1:(nrow(STK)))
K_STK = c(1:(nrow(STK)))
D_STK = c(1:(nrow(STK)))

for(i in 1:(KD_N-1)){
  Rsv_STK[i] = 0
  K_STK[i] = 0
  D_STK[i] = 0
}
K_STK[KD_N-1] = 50
D_STK[KD_N-1] = 50
for(i in KD_N:(nrow(STK))){
  Rsv_STK[i] = (Cl_STK[i]-min(Low_STK[(i-KD_N+1):i]))/(max(High_STK[(i-KD_N+1):i])-min(Low_STK[(i-KD_N+1):i])+0.0001)*100
  K_STK[i]=2/3*K_STK[i-1]+1/3*Rsv_STK[i]
  D_STK[i]=2/3*D_STK[i-1]+1/3*K_STK[i]
}

#EMA運算
#設定
EMA_N=5

EMA_N_STK=EMA(Cl_STK,EMA_N)

#MACD運算
#設定
EMA1=9
EMA2=12
EMA3=26

EMA12_STK=EMA(Cl_STK,EMA2)
EMA26_STK=EMA(Cl_STK,EMA3)
DIF_STK=EMA12_STK-EMA26_STK
MACD_STK=EMA(DIF_STK,EMA1)
DIF_MACD_STK=DIF_STK-MACD_STK

#William運算
William_11_STK = c(1:(nrow(STK)))
William_33_STK = c(1:(nrow(STK)))
William_89_STK = c(1:(nrow(STK)))

for(i in 1:(nrow(STK))){
  William_11_STK[i] = 0
  William_33_STK[i] = 0
  William_89_STK[i] = 0
}
for(i in 11:(nrow(STK))){
  William_11_STK[i] = 100-(max(High_STK[(i-11+1):i])-Cl_STK[i])/(max(High_STK[(i-11+1):i])-min(Low_STK[(i-11+1):i]))*100
  if(i>=33){
    William_33_STK[i] = 100-(max(High_STK[(i-33+1):i])-Cl_STK[i])/(max(High_STK[(i-33+1):i])-min(Low_STK[(i-33+1):i]))*100
  }
  if(i>=89){
    William_89_STK[i] = 100-(max(High_STK[(i-89+1):i])-Cl_STK[i])/(max(High_STK[(i-89+1):i])-min(Low_STK[(i-89+1):i]))*100
  }
}


#MTM運算
#設定
MTM_EMA_N=10
MTM_N = 10

MTM_EMA_N_STK=EMA(Cl_STK,MTM_EMA_N)
MTM_STK = c(1:(nrow(STK)))
m_MTM_STK = c(1:(nrow(STK))) #斜率
a_MTM_STK = c(1:(nrow(STK))) #加速度

for(i in 1:(nrow(STK))){
  MTM_STK[i] = 0
  m_MTM_STK[i] = 0
  a_MTM_STK[i] = 0
}
for(i in MTM_N:(nrow(STK))){
  MTM_STK[i] = Cl_STK[i]-Cl_STK[i-MTM_N+1]
  m_MTM_STK[i] = MTM_STK[i]/MTM_EMA_N_STK[i]
}
for(i in MTM_N+1:(nrow(STK))){
  a_MTM_STK[i] =  na.omit(m_MTM_STK[i] - m_MTM_STK[i-1])
}

#合併結果
result_STK <- data.frame(Date=Date_STK,
                         CL=Cl_STK,
                         OP=Op_STK,
                         HI=High_STK,
                         LO=Low_STK,
                         VO=Vol_STK,
                         RSV=Rsv_STK,
                         K=K_STK,
                         D=D_STK,
                         EMA5=EMA_N_STK,
                         EMA12=EMA12_STK,
                         EMA26=EMA26_STK,
                         DIF=DIF_STK,
                         MACD=MACD_STK,
                         DIF_MACD=DIF_MACD_STK,
                         William_11=William_11_STK,
                         William_33=William_33_STK,
                         William_89=William_89_STK,
                         MTM_EMA_10=MTM_EMA_N_STK,
                         MTM = MTM_STK,
                         m_MTM = m_MTM_STK,
                         a_MTM = a_MTM_STK
)
#輸出資料
save(result_STK, file="C:\\RData\\result_ETHUSDT_4HR_2017_8_16_2019_1_5.rda")
