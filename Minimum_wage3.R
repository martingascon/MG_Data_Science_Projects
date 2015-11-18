library(corrplot)
# 1) Does the minimum wage is creating/destroying employment?

# set working Directory
setwd('~/Dropbox/DS/Insight/Minimum_Wage/') 

# 1) 
############################################################################### Minimum Wage
# Download data for Min. Wages for states. Removed those which are mostly NA. 
# http://www.dol.gov/whd/state/stateMinWageHis.htm
# URL <-"http://www.dol.gov/whd/state/stateMinWageHis.htm"
# download.file(URL, destfile = "./data/MW.csv")
# The file had too many extra carachters. I manually removed them, and
# put them into Minimum_Wage.csv. I also removed states with NAs

# read the Minimum wage from  (1976 to 2014) 
mwage = read.table("./data/Minimum_Wage.csv", sep=",",head=TRUE)                            

# federal + 47 states. (Discarded States with Mostly NA)
rownames(mwage) <- mwage$State
colnames(mwage) <- sub("X", "\\2", colnames(mwage))
mwage[1]<-NULL

# 2) 
############################################################################  Unemployment
#http://www.bls.gov/lau/table14full14.xlsx (SEE ALSO)
#URL <- "http://www.bls.gov/lau/ststdsadata.txt"
#URL <- "http://www.bls.gov/lau/staadata.txt"
#download.file(URL, destfile = "./data/UN.csv", method="curl")
# The file had too many extra carachters. I manually removed them, and
# put them into Unemployment.csv

## Unemployment rate in the range (1976 to 2014) 
unem = read.csv("./data/Unemployment.csv", sep=" ",head=TRUE)     
rownames(unem)<-unem$State
colnames(unem) <- sub("X", "\\2", colnames(unem))
unem[1]<-NULL

# We dont have the average so let's add a row with the Federal average
unem["Federal" ,] <- colSums(unem)/nrow(unem)

## We have Federal + 49 states and dates from (1976 to 2014). 
# Let's select common states and years 
states<-rownames(unem[rownames(unem) %in% rownames(mwage), ])
years<-colnames(mwage[! colnames(mwage) %in% colnames(unem), ])
unem<-unem[states,years]

# Now let's remove the extra states from Minimum wage. 
states2<-rownames(mwage[rownames(mwage) %in% rownames(unem), ])
mwage<-mwage[states2,] 

#Let's reorder mwage with the same order as unem (states alph. and Federal last)
mwage<-mwage[match(rownames(unem),rownames(mwage)),]

############################################################################  UN AND MW FOR FEW STATES

#### let's plot the minimum wage for a few states compared to federal
par(mfrow = c(1, 2))
par(mar=c(5,4,4,2))

stats<-c("Federal","California","Colorado","Idaho","Kansas","Texas")
x<-as.numeric(colnames(mwage))
y<-t(mwage[stats,])
matplot(x,y, type="l", lwd=c(8,2,2,2,2,2), main = "Minimum wage",
        xlab="years", ylab="Minimum Wage in U$S", axes = FALSE)
axis(1, at = seq(1976, 2014, by = 1))
axis(2, at = seq(0, 10, by = 1))
legend("topleft", inset=0.01, as.vector(rownames(mwage[stats,])), col=c(1:8),
       lwd=c(8,2,2,2,2,2), cex=0.7)

# We can observe higher and lower values compared to the federal value.
##################################################################

##### let's plot the unemployment rate for a few states

par(mar=c(5,4,4,2))
x2<-colnames(unem)
y2<-t(unem[stats,])
matplot(x2,y2,type="l", lwd=c(8,2,2,2,2,2), main = "Unemployment Rate",
        xlab="years", ylab="Unemployment Rate (%)",axes=FALSE)
axis(1, at = seq(1976, 2014, by = 1),cex=0.7)
axis(2, at = seq(0, 15, by = 1))
legend("topleft", inset=0.01, as.vector(rownames(unem[stats,])), col=c(1:6),
       lwd=c(8,2,2,2,2,2,2,2), cex=0.7)

# We see a few increases starting in 2001 and later in 2008 due two economical crisis

############################################################################  CORR FOR FEDERAL & STATES
# Let's see if the federal unemployment is correlated with the federal minimum wage

par(mfrow = c(1, 2))
par(mar=c(5,4,4,4))
Sta<-"Federal"

y3<-t(unem[Sta,])
y4<-t(mwage[Sta,])
matplot(x2,y3, type="l", lwd=c(2),col=c(2), main = "U.S. (1976-2014)",
        xlab="year", ylab="Unemployment Rate (%)",axes=FALSE)
par(new=T)
plot(x2,y4, type="l", col="blue", lwd=c(2),axes=FALSE,xlab="",ylab="")
axis(1, at = seq(1976, 2016, by = 1),cex=0.7)
axis(2, at = seq(0, 10, by = 1))
axis(4, at = pretty(range(mwage[Sta,])), col="blue",col.lab="blue")
legend("top", inset=0.01, as.vector(c("Unemployment","Minimum Wage")), col=c("red","blue"),
       lwd=c(2,2))
mtext("Minimum wage ($)", side=4, line=2, col="blue")

cf<-cor(y3,y4)
legend("topleft", inset=0.01,c(paste("R=",round(cf, digits=2))),cex=0.7)


par(mar=c(5,4,4,4))
cor_state<-rep(0, 45)
for(i in 1:45) {
  cor_state[i]<-cor(as.numeric(unem[i,]),as.numeric(mwage[i,]))
}
mean<-summary(cor_state)[4] 
barplot(as.numeric(cor_state),xlab="State #",ylab="Correlation factor (R)", main="Correlation (44 states)")
abline(h=mean, col="black",lwd=2)
legend("topleft", inset=0.01,c(paste("R(Mean)=",round(mean, digits=2))), col=c(1),cex=0.7)


'''
par(mar=c(5,4,4,4))
plot(seq(1,45),cor_state, type="p", col="blue", lwd=c(2),pch=20,
     xlab="State #",ylab="Correlation factor", main="Correlation (44 states)")
abline(lm(cor_state~seq(1:45)), col="red",lwd=5) # regression line (y~x)
abline(h=0, col="black",lwd=2)
coef<-lm(cor_state~seq(1:45))
legend("bottomleft", inset=0.01,c(paste("Slope=",round(coef$coefficients[1], digits=2))), col=c("black"),cex=0.7)

# Very little correlation
'''

############################################################################  CONSIDERING GDP
#http://www.multpl.com/us-real-gdp-growth-rate/table/by-year
# We read the real GDP (inflation corrected) in % betweem 1976-2015

gdp = read.csv("./data/gdp.csv", sep=",",head=TRUE)     
rownames(gdp)<-as.numeric(gdp$year)
gdp[1]<-NULL
gdp<-gdp[years,]

par(mfrow = c(1, 1))
par(mar=c(5,4,4,4))
############################################################################  CORR FED UN. AND GDP
Sta<-"Federal"
x1<-as.numeric(colnames(unem)); 
y1<-t(unem[Sta,])
#y1<-predict(loess(t(unem[Sta,])~x1,span=0.40))
y2<-gdp
#y2<-predict(loess(y2~x2,span=0.35))
plot(x1,y1,type="l",lwd=c(2),col=c(2:8), xlab="year", ylab="Unemployment Rate (%)",
     main = "Unemployment vs GDP (US: 1976-2014)",axes=FALSE)
axis(1, at = seq(1976, 2016, by = 1),cex=0.7)
axis(2, at = pretty(range(y1)))
par(new=T)
plot(x1,y2, type="l", col="blue", lwd=c(2),axes=FALSE,xlab="",ylab="")
axis(4, at = pretty(range(y2)), col="blue",col.lab="blue")
legend("bottomlef", inset=0.01, as.vector(c("Unemployment","GDP (%)")), col=c("red","blue"),
       lwd=c(2,2), cex=0.7)
mtext("GDP (%)", side=4, line=2, col="blue")
cf<-cor(y1,y2)
legend("topleft", inset=0.01,c(paste("R=",round(cf, digits=2))), col=c("black"),cex=0.7)


# there is a negative correlation between GDP and unemployment. Why?
# A reduction in GDP means that employees are redundant and, 
# usually a dismissal tide will take place making the unemployment to rise
# but this effect is not immediate. occurs 1-2 years later in time?

############################################################################  UN VS 1/GDP
### first plot unemployment vs 1/gdp
par(mfrow = c(1, 1))
par(mar=c(5,4,4,4))
y3<-10*(1/(gdp+4))
plot(x1,y1,type="l",lwd=c(2),col=c(2:8), xlab="year", ylab="Unemployment Rate (%)",
     main = "Unemployment vs 1/GDP (US: 1976-2014)",axes=FALSE)
axis(1, at = seq(1976, 2016, by = 1),cex=0.7)
axis(2, at = pretty(range(y1)))
par(new=T)
plot(x1,y3, type="l", col="blue", lwd=c(2),axes=FALSE,xlab="",ylab="")
axis(4, at = pretty(range(y2)), col="blue",col.lab="blue")
legend("top", inset=0.01, as.vector(c("Unemployment","1/GDP")), 
       col=c("red","blue"),lwd=c(2,2), cex=0.7)
mtext("1/GDP (a.u.)", side=4, line=2, col="blue")
cf2<-cor(y1,y3)
legend("topleft", inset=0.01,c(paste("R=",round(cf2, digits=2))), col=c("black"),cex=0.7)


### lower plot

par(mar=c(5,4,4,4))
x4<-x1[3:26]
y4<-y3[1:24]
plot(x1,y1,type="l",lwd=c(2),col=c(2:8), xlab="year", ylab="Unemployment Rate (%)",
     main = "Unemployment vs 1/GDP (shifted)",axes=FALSE)
axis(1, at = seq(1976, 2016, by = 1),cex=0.7)
axis(2, at = pretty(range(y1)))
par(new=T)
plot(x4,y4, type="l", col="blue", lwd=c(2),axes=FALSE,xlab="",ylab="")
axis(4, at = pretty(range(y2)), col="blue",col.lab="blue")
legend("top", inset=0.01, as.vector(c("Unemployment","1/GDP (shifted 2 yr)")), 
       col=c("red","blue"),lwd=c(2,2), cex=0.7)
mtext("1/GDP (a.u.)", side=4, line=2, col="blue")
cf3<-cor(y1[3:26],y4)
legend("topleft", inset=0.01,c(paste("R=",round(cf3, digits=2))), col=c("black"),cex=0.7)


# Now, let's correct unemployment by dividing by GDP, so we remove this factor.

############################################################################   CORRECT UNEMPLOYMENT
## Since I have to displace two slots, unam corrected will have from 1980-2014
unem_corrected<-unem[,3:26]
for(i in 1:nrow(unem)) {
  unem_corrected[i,]<-unem_corrected[i,]/y2[1:24]
}

############################################################################  REPLOT CORR FED & STATES
# Let's see if the federal unemployment corrected is correlated with the MW
par(mfrow = c(1, 2))
par(mar=c(5,4,4,4))
Sta<-"Federal"

y3<-t(unem_corrected[Sta,])
y4<-t(mwage[Sta,3:26])
matplot(x4,y3, type="l", lwd=c(2),col=c(2), main = "UN & MW (1976-2014)",
        xlab="year", ylab="Unemployment Rate (u.a.)",axes=FALSE)
par(new=T)
plot(x4,y4, type="l", col="blue", lwd=c(2),axes=FALSE,xlab="",ylab="")
axis(1, at = seq(1976, 2016, by = 1),cex=0.7)
axis(2, at = seq(0, 10, by = 1))
axis(4, at = pretty(range(mwage[Sta,])), col="blue",col.lab="blue")
legend("bottomright", inset=0.01, as.vector(c("Unemployment (cor)","Minimum Wage")), col=c("red","blue"),
       lwd=c(2,2), cex=0.5)
mtext("Minimum wage ($)", side=4, line=2, col="blue")
cf5<-cor(y3,y4)
legend("topleft", inset=0.01,c(paste("R=",round(cf5, digits=2))), col=c("black"),cex=0.7)

cor_state2<-rep(0, 45)
for(i in 1:45) {
  cor_state2[i]<-(cor(as.numeric(unem_corrected[i,]),as.numeric(mwage[i,3:26])))
}

mean<-summary(cor_state2)[4] 
barplot(as.numeric(cor_state2),xlab="State #",ylab="Correlation factor", main="Correlation (44 states)")
abline(h=mean, col="black",lwd=2)
legend("top", inset=0.01,c(paste("Mean=",round(mean, digits=2))), col=c("black"),cex=0.7)



############################################################################  EXTRA
# Theoretically, if the minimum wage is a random number between 5 and 10$ 
# the correlation between MW and Unemployment should be 0. Here is the demostration.
 
N<-10000
cor_fed<-rep(0, N)
set.seed(289)
for(i in 1:N) {
  mw<-runif(26, 5.0, 10)
  cor_fed[i]<-cor(as.numeric(unem["Federal",]),as.numeric(mw))
}

par(mar=c(5,4,4,2))
hist(cor_fed, main = "Random Minimum Wage vs Federal Unemployment",xlab="Correlation factor")
prob<-pnorm(mean, mean=0, sd=sd(cor_fed) , lower.tail=FALSE) 
abline(v=mean,col="red",lwd=4)
mtext(at=c(0.5),c(paste("Prob =",round(prob*100, digits=1),"%")),col="red")

# Most of the States have a positive correlation


##################################################################### TRIED DIFF. DIDN'T WORK.
cor_state2<-rep(0, 45)
for(i in 1:44) {
  cor_state2[i]<-(cor(as.numeric(unem_dif[i,]),as.numeric(mwage_dif[i,])))
}
print(paste(rownames(unem_dif),round(cor_state2,2)))
summary(cor_state2)

### CALCULATION TO CHECK IF CORRELATION WORKS. 

#x1<-(unem_ave[4,]-mean(as.numeric(unem_ave[4,])))
#y1<-(mwage[4,]-mean(as.numeric(mwage[4,])))
#a<-sum(x1*y1)
#c<-sum(x1*x1)
#d<-sum(y1*y1)  
#a/sqrt(c*d)



## Let's plot differentials.  1992-1991, 1994-1992, ...,2014-2013. 
mwage_dif_vect<-as.vector(t(mwage_dif[1:44,]))
unem_dif_vect<-as.vector(t(unem_dif))

par(mfrow = c(1, 1))
par(mar=c(5.1,4.1,4.1,2.1))
x<-as.numeric(unem_dif_vect)
y1<-as.numeric( mwage_dif_vect)

plot(x, y1, main="Minimum Wage vs Unemployment rate",
     xlab="Unemplyoment rate (Delta %)", ylab="Minimum Wage (Delta $)", pch=19) 
#hist(x, y1, main="Minimum Wage vs Unemployment rate",
 #   xlab="Unemplyoment rate absolute difference", ylab="Minimum Wage difference ($)", breaks=10) 

abline(lm(y1~x), col="red") # regression line (y~x)


 


 
