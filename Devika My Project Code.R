library("psych", lib.loc="~/R/win-library/3.3")
describe(Cities42)

#Dependent Variable: RoomRent

#finding out the most important independent variables
cor(RoomRent,StarRating)
cor(RoomRent,IsTouristDestination)
cor(RoomRent,IsWeekend)
cor(RoomRent,IsMetroCity)
cor(RoomRent,Airport)
cor(RoomRent,Population)


aov(RoomRent~CityName)
summary(aov(RoomRent~CityName))
#CONCLUSION - THE MOST IMPORTANT INDEPENDENT VARIABLES: StarRating , IsTouristDestination, CityName

#1. Visualizing Y, x1, x2, x3 individually.
summary(RoomRent)
boxplot(RoomRent,main = "Room Rent Distribution")

summary(StarRating)
boxplot(StarRating,main = "Star Rating Distribution")

library("plyr", lib.loc="~/R/win-library/3.3")
count(Cities42,'StarRating')->FreqStarRating
count(Cities42,'IsTouristDestination')->freqTouristDest
count(Cities42,'CityName')->FreqCityName

#2. Visualising pair wise
mytableSR <- xtabs(~RoomRent+StarRating)
View(mytableSR)
summary(mytableSR)

#Call: xtabs(formula = ~RoomRent + StarRating)
#Number of cases in table: 13232 
#Number of factors: 2 
#Test for independence of all factors:
#  Chisq = 132395, df = 43100, p-value = 0
#Chi-squared approximation may be incorrect


mytableTD <- xtabs(~RoomRent+IsTouristDestination)
View(mytableTD)
summary(mytableTD)

#Call: xtabs(formula = ~RoomRent + IsTouristDestination)
#Number of cases in table: 13232 
#Number of factors: 2 
#Test for independence of all factors:
#  Chisq = 6413, df = 2155, p-value = 0
#Chi-squared approximation may be incorrect

mytableCN <- xtabs(~RoomRent+CityName)
summary(mytableCN)
#Call: xtabs(formula = ~RoomRent + CityName)
#Number of cases in table: 13232 
#Number of factors: 2 
#Test for independence of all factors:
#  Chisq = 237148, df = 88355, p-value = 0
#Chi-squared approximation may be incorrect

aov(RoomRent~IsTouristDestination)
summary(aov(RoomRent~IsTouristDestination))

aov(RoomRent~StarRating)
summary(aov(RoomRent~StarRating))

#Draw Scatter Plots to understand how are the variables correlated pair-wise
ggplot(Cities42, aes(x = CityRank, y = RoomRent)) +
geom_point(position = position_dodge(width = 0.4))

ggplot(Cities42, aes(x = IsTouristDestination, y = RoomRent)) +
geom_point(position = position_dodge(width = 0.4))
ggplot(Cities42, aes(x = StarRating, y = RoomRent)) +
geom_point(position = position_dodge(width = 0.4))


Data<-NULL
Data <- cbind(Data,CityName,IsTouristDestination,RoomRent,StarRating)
Data <- as.data.frame(Data)

#Create a Variance-Covariance Matrix for Y, x1, x2, x3

corrplot(corr=cor(Cities42[,c(3,5,10,11)], use="complete.obs"),
method ="ellipse",align = "center")

(corr=cor(Cities42[,c(3,5,10,11)], use="complete.obs"))
#CityRank IsTouristDestination   RoomRent StarRating
#CityRank              1.00000000            0.2807135 0.09398553 -0.1333810
#IsTouristDestination  0.28071345            1.0000000 0.12250296 -0.0405550
#RoomRent              0.09398553            0.1225030 1.00000000  0.3693734
#StarRating           -0.13338101           -0.0405550 0.36937343  1.0000000

#Draw a Corrgram of Y, x1, x2, x3  (Ignore other variables for now) 
corrgram(Cities42[,c(3,5,10,11)],upper.panel=panel.pie, text.panel=panel.txt,
main="Hotel Room Rent")


#tables 
xtabs(RoomRent~CityName+Airport,data = Cities42)->mytable
mytable<-as.data.frame(mytable)
names(mytable)
names(mytable)<-c("CityName","Airport","Price")
mytable<-as.data.frame(mytable[mytable$Price!= 0,])
View(mytable)

xtabs(RoomRent~CityName+Airport+IsTouristDestination,data = Cities42)->mytable2
mytable2<-as.data.frame(mytable2)
names(mytable2)<-c("CityName","Airport","IsTd","Price")
mytable2<-as.data.frame(mytable2[mytable2$Price!= 0 ,])
View(mytable2)

xtabs(RoomRent~CityName+Airport+IsTouristDestination+StarRating,data = Cities42)->mytable3
mytable3<-as.data.frame(mytable3)
names(mytable3)<-c("CityName","Airport","IsTd","StarRating","Price")
mytable3<-as.data.frame(mytable3[mytable3$Price!= 0 ,])
View(mytable3)

#Aggregate functions for finding means and sd
aggregate(Cities42$RoomRent , by=list(isTD = Cities42$IsTouristDestination), mean)
aggregate(Cities42$RoomRent , by=list(isTD = Cities42$IsTouristDestination), sd)

#fitting a linear model
fit<-lm(Price~CityName+Airport+IsTd+StarRating,data = mytable3)

summary(fit)




