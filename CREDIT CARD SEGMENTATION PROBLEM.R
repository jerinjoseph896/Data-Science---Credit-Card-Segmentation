library(dplyr)


#loading the data
setwd("C:/Users/jerin/Desktop/R work/EDWISOR PROJECT")

seg <-read.csv("CC GENERAL.csv")

View(seg)
sum(is.na(seg$CUST_ID))
sum(is.na(seg$BALANCE))
sum(is.na(seg$BALANCE_FREQUENCY))
sum(is.na(seg$PURCHASES))
sum(is.na(seg$ONEOFF_PURCHASES))
sum(is.na(seg$INSTALLMENTS_PURCHASES))
sum(is.na(seg$CASH_ADVANCE))
sum(is.na(seg$PURCHASES_FREQUENCY))
sum(is.na(seg$ONEOFF_PURCHASES_FREQUENCY))
sum(is.na(seg$PURCHASES_INSTALLMENTS_FREQUENCY))
sum(is.na(seg$CASH_ADVANCE_FREQUENCY))
sum(is.na(seg$CASH_ADVANCE_TRX))
sum(is.na(seg$PURCHASES_TRX))
sum(is.na(seg$CREDIT_LIMIT))##1
sum(is.na(seg$PAYMENTS))
sum(is.na(seg$MINIMUM_PAYMENTS))##313
sum(is.na(seg$PRC_FULL_PAYMENT))
sum(is.na(seg$TENURE))


# Identifying Outliers
mystats = function(x) {
  nmiss=sum(is.na(x))
  a = x[!is.na(x)]
  m = mean(a)
  n = length(a)
  s = sd(a)
  min = min(a)
  p1=quantile(a,0.01)
  p5=quantile(a,0.05)
  p10=quantile(a,0.10)
  q1=quantile(a,0.25)
  q2=quantile(a,0.5)
  q3=quantile(a,0.75)
  p90=quantile(a,0.90)
  p95=quantile(a,0.95)
  p99=quantile(a,0.99)
  max = max(a)
  UC = m+2*s
  LC = m-2*s
  outlier_flag= max>UC | min<LC
  return(c(n=n, nmiss=nmiss, outlier_flag=outlier_flag, mean=m, stdev=s,min = min, p1=p1,p5=p5,p10=p10,q1=q1,q2=q2,q3=q3,p90=p90,p95=p95,p99=p99,max=max, UC=UC, LC=LC ))
}


#New Variables creation# 

seg$Monthly_Avg_PURCHASES = seg$PURCHASES/(seg$PURCHASES_FREQUENCY*seg$TENURE)
seg$Monthly_CASH_ADVANCE = seg$CASH_ADVANCE/(seg$CASH_ADVANCE_FREQUENCY*seg$TENURE)
seg$LIMIT_USAGE = seg$BALANCE/seg$CREDIT_LIMIT
seg$MIN_PAYMENTS_RATIO = seg$PAYMENTS/seg$MINIMUM_PAYMENTS



Num_Vars = c(
  "BALANCE",
  "BALANCE_FREQUENCY",
  "PURCHASES",
  "Monthly_Avg_PURCHASES",
  "ONEOFF_PURCHASES",
  "INSTALLMENTS_PURCHASES",
  "CASH_ADVANCE",
  "Monthly_CASH_ADVANCE",
  "PURCHASES_FREQUENCY",
  "ONEOFF_PURCHASES_FREQUENCY",
  "PURCHASES_INSTALLMENTS_FREQUENCY",
  "CASH_ADVANCE_FREQUENCY",
  "CASH_ADVANCE_TRX",
  "PURCHASES_TRX",
  "CREDIT_LIMIT",
  "LIMIT_USAGE",
  "PAYMENTS",
  "MINIMUM_PAYMENTS",
  "MIN_PAYMENTS_RATIO",
  "PRC_FULL_PAYMENT",
  "TENURE")

Outliers=t(data.frame(apply(seg[Num_Vars], 2, mystats)))
View(Outliers)

write.csv(Outliers,"Outliers.csv")

# Outlier Treatment
seg$BALANCE[seg$BALANCE>5727.53]=5727.53
seg$BALANCE_FREQUENCY[seg$BALANCE_FREQUENCY>1.3510787]=1.3510787
seg$PURCHASES[seg$PURCHASES>5276.46]=5276.46
seg$Monthly_Avg_PURCHASES[seg$Monthly_Avg_PURCHASES>800.03] = 800.03
seg$ONEOFF_PURCHASES[seg$ONEOFF_PURCHASES>3912.2173709]=3912.2173709
seg$INSTALLMENTS_PURCHASES[seg$INSTALLMENTS_PURCHASES>2219.7438751]=2219.7438751
seg$CASH_ADVANCE[seg$CASH_ADVANCE>5173.1911125]=5173.1911125
seg$Monthly_CASH_ADVANCE[seg$Monthly_CASH_ADVANCE>2558.53] = 2558.53
seg$PURCHASES_FREQUENCY[seg$PURCHASES_FREQUENCY>1.2930919]=1.2930919
seg$ONEOFF_PURCHASES_FREQUENCY[seg$ONEOFF_PURCHASES_FREQUENCY>0.7991299]=0.7991299
seg$PURCHASES_INSTALLMENTS_FREQUENCY[seg$PURCHASES_INSTALLMENTS_FREQUENCY>1.1593329]=1.1593329
seg$CASH_ADVANCE_FREQUENCY[seg$CASH_ADVANCE_FREQUENCY>0.535387]=0.535387
seg$CASH_ADVANCE_TRX[seg$CASH_ADVANCE_TRX>16.8981202]=16.8981202
seg$PURCHASES_TRX[seg$PURCHASES_TRX>64.4251306]=64.4251306
seg$CREDIT_LIMIT[seg$CREDIT_LIMIT>11772.09]=11772.09
seg$LIMIT_USAGE[seg$LIMIT_USAGE>1.1683] = 1.1683
seg$PAYMENTS[seg$PAYMENTS>7523.26]=7523.26
seg$MINIMUM_PAYMENTS[seg$MINIMUM_PAYMENTS>5609.1065423]=5609.1065423
seg$MIN_PAYMENTS_RATIO[seg$MIN_PAYMENTS_RATIO>249.9239] = 249.9239
seg$PRC_FULL_PAYMENT[seg$PRC_FULL_PAYMENT>0.738713]=0.738713
seg$TENURE[seg$TENURE>14.19398]=14.19398


# Missing Value Imputation with mean 
seg$MINIMUM_PAYMENTS[which(is.na(seg$MINIMUM_PAYMENTS))] = 721.9256368
seg$CREDIT_LIMIT[which(is.na(seg$CREDIT_LIMIT))] = 4343.62
seg$Monthly_Avg_PURCHASES[which(is.na(seg$Monthly_Avg_PURCHASES))] =184.8991609
seg$Monthly_CASH_ADVANCE[which(is.na(seg$Monthly_CASH_ADVANCE))] = 717.7235629
seg$LIMIT_USAGE[which(is.na(seg$LIMIT_USAGE))] =0.3889264
seg$MIN_PAYMENTS_RATIO[which(is.na(seg$MIN_PAYMENTS_RATIO))]  = 9.3500701

# Checking Missing Value
check_Missing_Values=t(data.frame(apply(seg[Num_Vars], 2, mystats)))
View(check_Missing_Values)

write.csv(seg,"Missing_value_treatment.csv")



# Variable Reduction (Factor Analysis)

Step_nums = seg[Num_Vars]
corrm= cor(Step_nums)    
View(corrm)

write.csv(corrm, "Correlation_matrix.csv")


scree(corrm,factors=T,pc=T,main="scree plot", hline=NULL, add=FALSE)### SCREE PLOT


eigen(corrm)$values



eigen_values = mutate(data.frame(eigen(corrm)$values)
                      ,cum_sum_eigen=cumsum(eigen.corrm..values)
                      , pct_var=eigen.corrm..values/sum(eigen.corrm..values)
                      , cum_pct_var=cum_sum_eigen/sum(eigen.corrm..values))


write.csv(eigen_values, "EigenValues2.csv")


# standardizing the data
segment_prepared =seg[Num_Vars]

segment_prepared = scale(segment_prepared)
write.csv(segment_prepared, "standardized data.csv")

#building clusters using k-means clustering 
cluster_three = kmeans(segment_prepared,3)
cluster_four = kmeans(segment_prepared,4)
cluster_five = kmeans(segment_prepared,5)
cluster_six = kmeans(segment_prepared,6)

seg_new=cbind(seg,km_clust_3=cluster_three$cluster,km_clust_4=cluster_four$cluster,km_clust_5=cluster_five$cluster ,km_clust_6=cluster_six$cluster   )
View(seg_new)

# Profiling

Num_Vars2 = c(
  "Monthly_Avg_PURCHASES",
  "Monthly_CASH_ADVANCE",
  "CASH_ADVANCE",
  "CASH_ADVANCE_TRX",
  "CASH_ADVANCE_FREQUENCY",
  "ONEOFF_PURCHASES",
  "ONEOFF_PURCHASES_FREQUENCY",
  "PAYMENTS",
  "CREDIT_LIMIT",
  "LIMIT_USAGE",
  "PURCHASES_INSTALLMENTS_FREQUENCY",
  "PURCHASES_FREQUENCY",
  "INSTALLMENTS_PURCHASES",
  "PURCHASES_TRX",
  "MINIMUM_PAYMENTS",
  "MIN_PAYMENTS_RATIO",
  "BALANCE",
  "TENURE"
)




library(tables)

tt =cbind(tabular(1+factor(km_clust_3)+factor(km_clust_4)+factor(km_clust_5)+
                    factor(km_clust_6)~Heading()*length*All(seg[1]),
                  data=seg_new),tabular(1+factor(km_clust_3)+factor(km_clust_4)+factor(km_clust_5)+
                                          factor(km_clust_6)~Heading()*mean*All(seg[Num_Vars2]),
                                        data=seg_new))

tt2 = as.data.frame.matrix(tt)
View(tt2)

rownames(tt2)=c(
  "ALL",
  "KM3_1",
  "KM3_2",
  "KM3_3",
  "KM4_1",
  "KM4_2",
  "KM4_3",
  "KM4_4",
  "KM5_1",
  "KM5_2",
  "KM5_3",
  "KM5_4",
  "KM5_5",
  "KM6_1",
  "KM6_2",
  "KM6_3",
  "KM6_4",
  "KM6_5",
  "KM6_6")


colnames(tt2)=c(
  "SEGMENT_SIZE",
  "Monthly_Avg_PURCHASES",
  "Monthly_CASH_ADVANCE",
  "CASH_ADVANCE",
  "CASH_ADVANCE_TRX",
  "CASH_ADVANCE_FREQUENCY",
  "ONEOFF_PURCHASES",
  "ONEOFF_PURCHASES_FREQUENCY",
  "PAYMENTS",
  "CREDIT_LIMIT",
  "LIMIT_USAGE",
  "PURCHASES_INSTALLMENTS_FREQUENCY",
  "PURCHASES_FREQUENCY",
  "INSTALLMENTS_PURCHASES",
  "PURCHASES_TRX",
  "MINIMUM_PAYMENTS",
  "MIN_PAYMENTS_RATIO",
  "BALANCE",
  "TENURE"
)

tt2
cluster_profiling2 = t(tt2)

