library(quantmod)
library(TTR)
library(xts)
library(plyr)

load(file="C:\\RData\\result_BTCUSDT_5MIN_2017_8_16_2019_1_5.rda")

take_profit=c(1.1,1.2,1.3)
stop_loss=c(0.95,0.9,0.85)

max_earn11=c()
max_try_times11=c()
min_earn11=c()
min_try_times11=c()

max_earn12=c()
max_try_times12=c()
min_earn12=c()
min_try_times12=c()

max_earn13=c()
max_try_times13=c()
min_earn13=c()
min_try_times13=c()

for(j in 1:3){
  max_earn=c()
  max_try_times=c()
  min_earn=c()
  min_try_times=c()
  for(k in 1:14){
    #STK=result_STK[88:1087,]
    STK=result_STK[(88+(k-1)*10000):(10088+(k-1)*10000-1),]
    #(1)2017-08-17 06:15:00 EST 88-10087  (14)130088-140087 2018-12-15 19:50:00 EST
    #STK=result_STK[145254:146253,]
    Cl_STK = STK[,"CL"]
    Op_STK = STK[,"OP"]
    K_STK = STK[,"K"]
    D_STK = STK[,"D"]
    
    buy_loc = c()
    sell_loc = c()
    check_buy=1
    for(i in 2:nrow(STK)){
      if(check_buy==1 && K_STK[i]>D_STK[i] && K_STK[i-1]<D_STK[i-1] && K_STK[i]<30){
        buy_loc = c(buy_loc,i)
        check_buy = 0
      }
      if(check_buy==0 && K_STK[i]<D_STK[i] && K_STK[i-1]>D_STK[i-1] && K_STK[i]>80){
        sell_loc = c(sell_loc,i)
        check_buy = 1
      }
    }
    
    store_total_price_qty = c()
    for(m in 1 : 20){
      buy_loc_index=1
      total_price_qty=0
      total_qty=0
      try_times=m
      first_buy_price=0
      per_qty=1
      for(i in 1:nrow(STK)){
        if(length(buy_loc)<buy_loc_index || length(buy_loc) <= 0 || length(sell_loc)<buy_loc_index || length(sell_loc) <=0){
          break
        }
        current_price_op = Op_STK[i] 
        if(i == (buy_loc[buy_loc_index]+1)){
          bought_price=current_price_op
          first_buy_price=bought_price
          total_qty=total_qty+per_qty
          total_price_qty=total_price_qty+bought_price*per_qty
        }
        if(i >= buy_loc[buy_loc_index]+2 && i <= buy_loc[buy_loc_index]+try_times && i < sell_loc[buy_loc_index]){
          bought_price=current_price_op
          if(first_buy_price<=bought_price){
            total_qty=total_qty+per_qty
            total_price_qty=total_price_qty+bought_price*per_qty
          }
        }
        if(i == (sell_loc[buy_loc_index]+1)){
          sell_price=current_price_op
          total_price_qty=total_price_qty-sell_price*total_qty
          total_qty=0
          buy_loc_index=buy_loc_index+1
        }
        if(total_qty!=0 && current_price_op < first_buy_price*stop_loss[j]){
          sell_price=current_price_op
          total_price_qty=total_price_qty-sell_price*total_qty
          total_qty=0
          buy_loc_index=buy_loc_index+1
        }
        if(total_qty!=0 && current_price_op > first_buy_price*take_profit[j]){
          sell_price=current_price_op
          total_price_qty=total_price_qty-sell_price*total_qty
          total_qty=0
          buy_loc_index=buy_loc_index+1
        }
      }
      store_total_price_qty=c(store_total_price_qty,total_price_qty/(m*per_qty))
    }
    #head(store_total_price_qty,20) #每個qty賺的
    max_earn=c(max_earn,max(store_total_price_qty))
    max_try_times=c(max_try_times,which.max(store_total_price_qty))
    min_earn=c(min_earn,min(store_total_price_qty))
    min_try_times=c(min_try_times,which.min(store_total_price_qty))
  }
  if(j==1){
    max_earn11=c(max_earn11,max_earn)
    max_try_times11=c(max_try_times11,max_try_times)
    min_earn11=c(min_earn11,min_earn)
    min_try_times11=c(min_try_times11,min_try_times)
  }else if(j==2){
    max_earn12=c(max_earn12,max_earn)
    max_try_times12=c(max_try_times12,max_try_times)
    min_earn12=c(min_earn12,min_earn)
    min_try_times12=c(min_try_times12,min_try_times)
  }else if(j==3){
    max_earn13=c(max_earn13,max_earn)
    max_try_times13=c(max_try_times13,max_try_times)
    min_earn13=c(min_earn13,min_earn)
    min_try_times13=c(min_try_times13,min_try_times)
  }
}
#head(max_earn,14)
#head(max_try_times,14)
#head(min_earn,14)
#head(min_try_times,14)

#合併結果
result_module_KD <- data.frame(Time_ID=c(1:14),
                               max_earn11=max_earn11,
                               max_try_times11=max_try_times11,
                               min_earn11=min_earn11,
                               min_try_times11=min_try_times11,
                               max_earn12=max_earn12,
                               max_try_times12=max_try_times12,
                               min_earn12=min_earn12,
                               min_try_times12=min_try_times12,
                               max_earn13=max_earn13,
                               max_try_times13=max_try_times13,
                               min_earn13=min_earn13,
                               min_try_times13=min_try_times13
)




