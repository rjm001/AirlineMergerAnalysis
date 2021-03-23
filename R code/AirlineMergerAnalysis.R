#######################################################
# Airline Merger Analysis
######################################################

library(here)
library(tidyverse)
profits = read.table(here("AirlineData", "airlineprofits.txt"), header=T)
revenue = read.table(here("AirlineData", "airlinerevenues.txt"), header=T)
head(profits)
head(revenue)

##########################################################
#first profits
##########################################################


xval = seq(from = 2000.25,length = nrow(profits), by = .25)
plot(xval, profits[,3])

fit = list(10)
fit[[1]] = lm(profits[,3]~xval)
col.vec = rainbow( (ncol(profits)-3) )
for(i in 4:ncol(profits)){
  fit[[(i - 2)]] = lm(profits[,i]~xval)
  lines(xval,profits[,i],col = col.vec[i])
}

i=11
name = names(profits)
plot(xval,profits[,i], main = name[i],xlab = 'Year',ylab = 'profits',lty = 2,type = 'l')
abline(fit[[i-2]],col = 'red')
legend(x = 'bottomright',c("data","OLS"),lty = c(2,1),lwd = c(2.5,2.5), col = c("black","red"))


for(i in 1:(ncol(profits)-2)){

print(summary(fit[[i]]))
print(i)
  
}  


############################333
#shortening
##############################

profits.s = profits[41:nrow(profits),]
xval.s = seq(from = 2010.25,length = nrow(profits.s), by = .25)
plot(xval.s, profits.s[,3])

fit.s = list(10)
fit.s[[1]] = lm(profits.s[,3] ~xval.s)
col.vec = rainbow( (ncol(profits.s)-3) )
for(i in 4:ncol(profits.s)){
  fit.s[[(i - 2)]] = lm(profits.s[,i]~xval.s)
  lines(xval.s,profits.s[,i],col = col.vec[i])
}
col.vec
i=12
plot(xval.s,profits.s[,i])
abline(fit.s[[i-2]])


for(i in 1:(ncol(profits.s)-2)){
  print(summary(fit.s[[i]]))
  print(i)
}  

##########################################################
#second, revenue
##########################################################

fit.r = list(10)
plot(xval, revenue[,3])
fit.r[[1]] = lm(revenue[,3]~xval)
col.vec = rainbow( (ncol(revenue)-3) )
for(i in 4:ncol(revenue)){
  fit.r[[(i - 2)]] = lm(revenue[,i]~xval)
  lines(xval,revenue[,i],col = col.vec[i])
}

i=12
name = names(revenue)
plot(xval,revenue[,i], main = name[i],xlab = 'Year',ylab = 'revenue',lty = 2,type = 'l')
abline(fit.r[[i-2]],col = 'red')
legend(x = 'bottomright',c("data","OLS"),lty = c(2,1),lwd = c(2.5,2.5), col = c("black","red"))


for(i in 1:(ncol(revenue)-2)){
  print(summary(fit.r[[i]]))
  print(i)
}


##########################################################
#pricing data
##########################################################

prices = read.table(here("AirlineData", "DB1B2010Q1.csv"),header = T, sep = ",")

head(prices)
price = prices[,-ncol(prices)]
head(price)

sum((price[,13]<300)&(price[,13]<250))
condition1 = ((price[,13]<300)&(price[,13]>250))&(price[,11]<10000)&(price[,11]>0)
flight1 = price[condition1,]
plot(ecdf(flight1[,11]))
plot(density(flight1[,11]))
boxplot(flight1[,11])

  condition1.1 = condition1&(prices[,9] == "US") #us air
  condition1.2 = condition1&(prices[,9] == "AA") #American
  condition1.3 = condition1&(prices[,9] == "DL") #delta
  condition1.4 = condition1&(prices[,9] == "WN") #Southwest
  condition1.5 = condition1&(prices[,9] == "VX") #Virgin
  condition1.6 = condition1&(prices[,9] == "UA") #United

  lines(ecdf(prices[condition1.1,11]), col  = 'red')
  lines(ecdf(prices[condition1.2,11]), col  = 'blue')
  lines(ecdf(prices[condition1.3,11]), col  = 'red')
  lines(ecdf(prices[condition1.4,11]), col  = 'purple')
  lines(ecdf(prices[condition1.5,11]), col  = 'orange')
  lines(ecdf(prices[condition1.6,11]), col  = 'yellow')

  mean(prices[condition1.1,11])
  mean(prices[condition1.2,11])
  mean(prices[condition1.3,11])
  mean(prices[condition1.4,11])
  mean(prices[condition1.5,11])
  mean(prices[condition1.6,11])

mean(prices[condition1.1,11]) - sd(prices[condition1.1,11])/sqrt(length(prices[condition1.1,11]))*1.96

condition2 = ((price[,13]<350)&(price[,13]>=300))&(price[,11]<12000)&(price[,11]>0)
flight1 = price[condition2,]
plot(ecdf(flight1[,11]))
plot(density(flight1[,11]))
boxplot(flight1[,11])

condition3 = ((price[,13]<400)&(price[,13]>=300))&(price[,11]<15000)&(price[,11]>0)
flight1 = price[condition3,]
plot(ecdf(flight1[,11]))
plot(density(flight1[,11]))
boxplot(flight1[,11])


################################################
################################################
#looking at merger city pairs
#################################################

rm(list() = ls)
#my data
mystr = list(here( "AirlineData","DB1B2014Q4.csv"),
  here( "AirlineData","DB1B2014Q3.csv"),
  here( "AirlineData","DB1B2014Q2.csv"),
  here( "AirlineData","DB1B2014Q1.csv"),
  here( "AirlineData", "DB1B2013Q4.csv"),
  here( "AirlineData", "DB1B2013Q3.csv"),
  here( "AirlineData", "DB1B2013Q2.csv"),
  here( "AirlineData", "DB1B2013Q1.csv"),
  here( "AirlineData","DB1B2012Q4.csv"),
  here( "AirlineData", "DB1B2012Q3.csv"),
  here( "AirlineData", "DB1B2012Q2.csv"),
  here( "AirlineData", "DB1B2012Q1.csv"),
  here( "AirlineData", "DB1B2011Q4.csv"),
  here( "AirlineData", "DB1B2011Q3.csv"),
  here( "AirlineData", "DB1B2011Q2.csv"),
  here( "AirlineData", "DB1B2011Q1.csv"),
  here( "AirlineData", "DB1B2010Q4.csv"),
  here( "AirlineData", "DB1B2010Q3.csv"),
  here( "AirlineData", "DB1B2010Q2.csv"),
  here( "AirlineData","DB1B2010Q1.csv"))

citypair = read.table(here( "AirlineData","citypairs.txt"),
                    header = T)

citylist = citypair[,]

cities = nrow(citylist)
alpha = .5
out = matrix(0,20,cities)
out.AA = matrix(0,20,cities)
out.US = matrix(0,20,cities)
out.OT = matrix(0,20,cities)
out.DL = matrix(0,20,cities)
out.WN = matrix(0,20,cities)  #southwest
out.UA = matrix(0,20,cities)
#cond.comp = c(T,nrow(price)) #to find which city pairs never get called
    #actually, I would need 20 of these, if I had the memory
for(i in 1:20){
  price = (read.table(mystr[[i]], header= T, sep = ","))[,c(4,7,9,11,13)]
  for (j in 1:cities) {
    cond = ((as.character(price[,1])==as.character(citylist[j,1]))&
              (as.character(price[,2])== as.character(citylist[j,2])))|
      ((as.character(price[,1])== as.character(citylist[j,2]))&
         (as.character(price[,2])== as.character(citylist[j,1])))
    out[i,j] = quantile(price[cond,4],probs = alpha)
    out.AA[i,j] = quantile(price[(cond&(price[,3] == "AA") ) ,4],probs = alpha)
    out.US[i,j] = quantile(price[(cond&(price[,3] == "US") ) ,4],probs = alpha)
    out.DL[i,j] = quantile(price[(cond&(price[,3] == "DL") ) ,4],probs = alpha)
    out.UA[i,j] = quantile(price[(cond&(price[,3] == "UA") ) ,4],probs = alpha)
    out.WN[i,j] = quantile(price[(cond&(price[,3] == "WN") ) ,4],probs = alpha)
    out.OT[i,j] = quantile(price[(cond&(price[,3] != "US")&(price[,3] != "WN")&
                  (price[,3] != "UA")&(price[,3] != "AA")&
                    (price[,3] != "DL")) ,4],probs = alpha)
  }
  
  rm(cond)
  if(i < 20) {
    rm(price)
  }
  print(i)
}

#determining list of distances
dist = rep(0,cities)
for (j in 1:cities) {
  cond = ((as.character(price[,1])==as.character(citylist[j,1]))&
          (as.character(price[,2])== as.character(citylist[j,2])))|
  ((as.character(price[,1])== as.character(citylist[j,2]))&
        (as.character(price[,2])== as.character(citylist[j,1])))

  dist[j] = price[which(cond)[1],5]
  if (j %% 50 ==0) {
    print(j)
  }           
}

write(t(dist), here( "AirlineData","distance.txt"), 1)
write(out, here( "AirlineData","out.txt"), 20) #and rest
picked = sample(1:cities, 30, replace = F)
write(picked, here( "AirlineData","picked.txt"), 1)

citydist = cbind(citylist,dist)

w = picked[30]  
for (w in picked) {
xval.s = seq(from = 2010.25,to = 2015, by = .25)
yval.s = seq(from = 0, to = 500, length = length(xval.s))
plot(xval.s, yval.s, main = paste("Median Price Between City Pairs ", 
                    citylist[w,1], " and ", citylist[w,2], " by Carrier and Quarter", 
                    sep = ""), type = 'l', lty = 2, col = "White",
                    xlab = "Quarter", ylab = "Media price"       )
lines(xval.s,out[,w], col = 'Black',lty = 1)
lines(xval.s,out.AA[,w],col = 'Blue',lty = 3)
lines(xval.s,out.US[,w],col = 'Orange',lty = 4)
lines(xval.s,out.OT[,w],col = 'Green', lty = 5)
lines(xval.s,out.DL[,w],col = 'Red', lty = 2)
lines(xval.s,out.UA[,w],col = 'brown',lty = 6)
lines(xval.s,out.WN[,w],col = 'purple', lty = 7)

legend(x = "topright",c("total","AA","US", "Delta","United",
                   "S West", "Other"),lty = c(1,3,4,2,6,7,5),
       lwd = c(2.5,2.5,2.5,2.5), col = c("black",
          "blue","orange","red","brown","purple","green") )


legend(x = "top",c("total","AA","US", "Delta",
                   "S West", "Other"),lty = c(1,3,4,2,7,5),
       lwd = c(2.5,2.5,2.5,2.5), col = c("black",
                    "blue","orange","red","purple","green") )



legend(x = "top",c("total","AA","US", "Delta","United","Southwest"),
       lty = c(1,3,4,2,6,7),lwd = c(2.5,2.5,2.5,2.5), 
       col = c("black","blue","orange","red","brown","purple") )


legend(x = 'top',c("total","AA","US", "Delta","United","Other"),
       lty = c(1,3,4,2,6,5),lwd = c(2.5,2.5,2.5,2.5), 
       col = c("black","blue","orange","red","brown","green") )


legend(x = 'bottomleft',c("total","AA","US", "Delta","Other"),
       lty = c(1,3,4,2,5),lwd = c(2.5,2.5,2.5,2.5), 
       col = c("black","blue","orange","red","green") )

legend(x = 'bottomleft',c("total","AA", "United","Other"),
       lty = c(1,3,6,5),lwd = c(2.5,2.5,2.5,2.5), 
       col = c("black","blue","brown","green") )



legend(x = 'bottomleft',c("total","US", "AA","United","Other"),
       lty = c(1,4,3,6,5),lwd = c(2.5,2.5,2.5,2.5), 
       col = c("black","orange","blue","brown","green") )




legend(x = 'top',c("total","US", "Delta","United","Other"),
       lty = c(1,4,2,6,5),lwd = c(2.5,2.5,2.5,2.5), 
       col = c("black","orange","red","brown","green") )


legend(x = 'top',c("total","US", "Delta","Other"),
       lty = c(1,4,2,5),lwd = c(2.5,2.5,2.5,2.5), 
       col = c("black","orange","red","green") )

legend(x = 'top',c("total","US", "United", "Other"),
       lty = c(1,4,6,5),lwd = c(2.5,2.5,2.5,2.5), 
       col = c("black","orange","brown","green") )

legend(x = 'top',c("total","US",  "Other"),
       lty = c(1,4,5),lwd = c(2.5,2.5,2.5,2.5), 
       col = c("black","orange","green") )


legend(x = 'top',c("total","Delta", "Other"),
       lty = c(1,4,6,5),lwd = c(2.5,2.5,2.5,2.5), 
       col = c("black","orange","brown","green") )

legend(x = 'bottom',c("total","AA","US", "Delta","United"),
       lty = c(1,3,4,2,6),lwd = c(2.5,2.5,2.5,2.5), 
       col = c("black","blue","orange","red","brown") )


legend(x = 'top',c("total","AA","US", "Delta","S West"),
       lty = c(1,3,4,2,6),lwd = c(2.5,2.5,2.5,2.5), 
       col = c("black","blue","orange","red","purple" ))



}
nas = !is.na(dist)

cd.s = citydist[nas,]
max(cd.s[,3])
min(cd.s[,3])



###########################################################
#looking for city groups with similar distances
###########################################################
sup = seq(from = 6, to = 6506, by = 500)

length(levels(cd.s[,1]))

hist(cd.s[,3], breaks = sup)
#note that there are none in the 5500-6000 range
medpric = matrix(0,20,(length(sup)-1))
ctlevels = rep(0,13)
i = 20
for(i in 1:19){
  price = (read.table(mystr[[i]], header= T, sep = ","))[,c(4,7,9,11,13)]
  
  
  for (j in 1:(length(sup)-1) ) {
    cond= (price[,5]>sup[j])&(price[,5]<sup[(j+1)])
    #medpric[i,j] = quantile(price[cond,4],prob = alpha)
    if (i ==20) {
    ctlevels[j] = length(levels(price[cond,1]))
    }
  }  
  rm(cond)
  if(i < 20) {
    rm(price)
  }
  print(i)
}

write(t(medpric), here( "AirlineData","medpric.txt"), 20)
length(levels(price[cond,1]))

#median change within group for our set of travels
#total, AA, US, Delta,United
out.big = array(0,c(20,13,5))

for(i in 1:20) {
  for (j in 1:(length(sup)-1) ) {
    cond= (cd.s[,3]>sup[j])&(cd.s[,3]<sup[(j+1)])
    out.big[i,j,1] = quantile(out[i,cond],alpha,na.rm = T)
    out.big[i,j,2] = quantile(out.AA[i,cond],alpha,na.rm = T)
    out.big[i,j,3] = quantile(out.US[i,cond],alpha,na.rm = T)
    out.big[i,j,4] = quantile(out.DL[i,cond],alpha,na.rm = T)
    out.big[i,j,5] = quantile(out.UA[i,cond],alpha,na.rm = T)
  } #close j
} #close i


bigbind = rbind(out.big[,,1],out.big[,,2],out.big[,,3],out.big[,,4],out.big[,,5])
write(t(bigbind),here( "AirlineData","outbig.txt"),13)

######################################################
#plotting price sequences
######################################################

w = 8
yval.s = seq(from = 0, to = 500, length = length(xval.s))
plot(xval.s, yval.s, main = paste("Median Price for City Pairs between ", 
                                  sup[w], " and ", sup[(w+1)], " Miles Apart", 
                                  sep = ""), type = 'l', lty = 2, col = "White",
     xlab = "Quarter", ylab = "Median price"       )
lines(xval.s,out.big[,w,1], col = 'Black',lty = 1)
lines(xval.s,out.big[,w,2],col = 'Blue',lty = 3)
lines(xval.s,out.big[,w,3],col = 'Orange',lty = 4)
lines(xval.s,out.big[,w,4],col = 'red', lty = 5)
lines(xval.s,out.big[,w,5],col = 'brown', lty = 2)
lines(xval.s,medpric[,w],col = 'green',lty = 6)
legend(x = "bottom",c("Total in Suspect","AA in Suspect","US in Suspect", 
                  "Delta in Suspect","United in Suspect", "Avg for all"),lty = c(1,3,4,5,2,6),
          col = c("black","blue","orange","red","brown","green" ))
       



