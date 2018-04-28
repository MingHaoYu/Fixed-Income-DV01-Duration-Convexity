#408-Fixed Income HW3
#Author: Ming-Hao Yu
#Date: 2018/04/25

library(data.table)

setwd("C:/Users/Ming-Hao/Desktop/MFE/408-Fixed Income")
dt = fread(file="Homework 3 Data.csv", skip=3)
DT = dt$V2
y = 2*(DT^(-1/(2*seq(1,length(DT)))) - 1)
tList = seq(0.5, 25, 0.5)
#Q1
c = 2*(100-100*DT)/cumsum(DT)/100
plot(y=c, x=tList, xlab="Maturity (Years)", ylab="Yield", main="Semiannual bonds Par rates", type="l")

#Q2
DV01 = vector()
for(i in 1:50) {
    coupon = 100*c[i]
    ii = head(seq(1, i), -1)
    Price1 = sum(coupon/2*DT[ii]) + (100+coupon/2)*DT[i]
    y2 = 2*(DT^(-1/(2*seq(1,length(DT)))) - 1) + 0.0001/2
    DT2 = 1/(1+y2/2)^(2*seq(1,length(DT)))
    Price2 = sum(coupon/2*(DT2[ii])) + (100+coupon/2)*(DT2[i])
    DV01[i] = - (Price2 - Price1)
}

plot(y=DV01, x=tList, xlab="Maturity (Years)", ylab="DV01", main="Semiannual bonds DV01", type="l")

#Q3
MC = vector()
for(i in 2:10){
    ii = seq(1,i-1)
    MC[i] = (sum( ii/2*100*c[i]/2*DT[ii] ) + i/2*(100+100*c[i]/2)*DT[i])/100
}
MD = MC/(1+c[1:10]/2)

Q3_MC = MC[seq(2,10,2)]
Q3_MD = MD[seq(2,10,2)]
names(Q3_MC) =  names(Q3_MD) = c("1 year", "2 year", "3 year", "4 year", "5 year")
plot(y=Q3_MC, x=seq(1,5), xlab="Maturity (Years)", ylab="Duration (Years)", 
     main="Macauley Duration & Modified Duration", type="l")
points(y=Q3_MD, x=seq(1,5), type="l", col="red")
legend("topleft", c("Macauley Duration", "Modified Duration"), col=c("black", "red"), cex=0.8, lwd=1)

#Q4 
L = 5e6*DT[6]
#Invest L in zero-coupon bond which gets paid $5000000 in 3rd year.

#Q5
Convexity = vector()
for(i in 1:10){
    ii = head(seq(1,i),-1)
    Convexity[i] = (sum( ii*(ii+1)*100*c[i]/2*DT[ii] ) + i/2*(100+100*c[i]/2)*DT[i])/(1+c[i]/2)^2/2^2/100
}
Q5 = Convexity[seq(2,10,2)]
names(Q5) = c("1 year", "2 year", "3 year", "4 year", "5 year")
plot(y=Q5, x=seq(1,5), xlab="Maturity (Years)", ylab="Yield", main="Semiannual bonds Convexity", type="l")

#Q6
#dP = -P*MD*dr + 0.5*P*Convexity*dr^2
dp_upward = -100*Q3_MD*100/10000 + 0.5*100*Q5*(100/10000)^2
dp_downward = -100*Q3_MD*(-100)/10000 + 0.5*100*Q5*(-100/10000)^2
dp_upward_actual = dp_downward_actual = dp1 = dp2 = vector()
for(i in 1:10) {
    coupon = 100*c[i]
    ii = head(seq(1, i), -1)
    y2 = 2*(DT^(-1/(2*seq(1,length(DT)))) - 1) + 0.01/2
    DT2 = 1/(1+y2/2)^(2*seq(1,length(DT)))
    y3 = 2*(DT^(-1/(2*seq(1,length(DT)))) - 1) - 0.01/2
    DT3 = 1/(1+y3/2)^(2*seq(1,length(DT)))
    Price2 = sum(coupon/2*(DT2[ii])) + (100+coupon/2)*(DT2[i])
    Price3 = sum(coupon/2*(DT3[ii])) + (100+coupon/2)*(DT3[i])
    dp1[i] = Price2 - 100
    dp2[i] = Price3 - 100 
}
dp_upward_actual = dp1[seq(2,10,2)]
dp_downward_actual = dp2[seq(2,10,2)]

ylim = c(min(dp_upward, dp_upward_actual), 
         max(dp_upward, dp_upward_actual))
plot(y=dp_upward, x=seq(1,5), main="Price change due to yield +100 basis", 
     xlab="Maturity (years)", ylab="Price Change ($)", type="l", col="red", ylim=ylim)
points(y=dp_upward_actual, x=seq(1,5), type="l", col="blue")
legend("topright", c("Duration-Convexity approach", "Actual Change"), col=c("red", "blue"), cex=0.8, lwd=1)

Q6_up = matrix(nrow=2, ncol=length(dp_upward), c(dp_upward, dp_upward_actual), byrow=T)
colnames(Q6_up) = c("1 year", "2 year", "3 year", "4 year", "5 year")
rownames(Q6_up) = c("Duration-Convexity", "Actual Change")
Q6_up

ylim = c(min(dp_downward, dp_downward_actual), 
         max(dp_downward, dp_downward_actual))
plot(y=dp_downward, x=seq(1,5), main="Price change due to yield -100 basis", 
     xlab="Maturity (years)", ylab="Price Change ($)", type="l", col="red", ylim=ylim)
points(y=dp_downward_actual, x=seq(1,5), type="l", col="blue")
legend("bottomright", c("Duration-Convexity approach", "Actual Change"), col=c("red", "blue"), cex=0.8, lwd=1)

Q6_down = matrix(nrow=2, ncol=length(dp_downward), c(dp_downward, dp_downward_actual), byrow=T)
colnames(Q6_down) = c("1 year", "2 year", "3 year", "4 year", "5 year")
rownames(Q6_down) = c("Duration-Convexity", "Actual Change")
Q6_down
