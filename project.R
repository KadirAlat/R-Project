##Countries.csv has been taken from kaggle.com
#It includes some information about footprint
#i will try to conclude some information from dataset

x <- read.csv("countries.csv",header = T,sep = ",")

x<-data.frame(x)

x<-na.omit(x)
#is HDI related with responsibility to to environment?


averagehdi <- sum(x$HDI)/length(x$HDI)+0.2 #(to get more clear result, i increased the limit of development index)

Name_of_Developed_Countries <- x[x$HDI>averagehdi,1]

Name_of_Developed_Countries

m<- x[x$HDI>averagehdi,2]

#Region of developed countries

table(m)

barplot(table(m),main = "Numof Countries of Continent which have more than average HDI",col=rainbow(6),names.arg = c("Africa","Asia-Pasific","European Union","Latin America","MiddleEast/C.A","North America","N/E.Europe"))

#european union is clearly beyond the others..


EcologicalFootprintForEuropeanUnion <- x[x$Region=='European Union',11]
PopulationForEuropeanUnion <- x[x$Region=='European Union',3]*10
NamesForEuropeanUnion <- x[x$Region=='European Union',1]
average_rate_europeanUnion <- sum(rate_of_ecologicalfootprint_population)/length(rate_of_ecologicalfootprint_population)
rate_of_ecologicalfootprint_population <- EcologicalFootprintForEuropeanUnion/PopulationForEuropeanUnion
barplot(rate_of_ecologicalfootprint_population,names.arg = NamesForEuropeanUnion,space = 0,col = rainbow(20),cex.names = .60,las=2,main = "Rate of Echological Footprint Graph for European Union Countries",xlab = "Countries",ylab = "RateofFootprint",cex.axis = 1)
abline(h=average_rate_europeanUnion,col="black", lwd=2)
text(x=21,y=0.3,"Average=",pos = 4)
text(x=23,y=0.3,round(average_rate_europeanUnion,3),pos = 4)



EcologicalFootprintForAfrica <- x[x$Region=='Africa',11]
PopulationForAfrica <- x[x$Region=='Africa',3]*10
NamesForAfrica <- x[x$Region=='Africa',1]
average_rate_Africa <- sum(rate_of_ecologicalfootprint_populationAfrica)/length(rate_of_ecologicalfootprint_populationAfrica)
rate_of_ecologicalfootprint_populationAfrica <- EcologicalFootprintForAfrica/PopulationForAfrica
barplot(rate_of_ecologicalfootprint_populationAfrica,names.arg = NamesForAfrica,space = 0,col = rainbow(20),cex.names = .60,las=2,main = "Rate of Echological Footprint Graph for African Countries",xlab = "Countries",ylab = "RateofFootprint",cex.axis = 1)
abline(h=average_rate_europeanUnion,col="black", lwd=2)
text(x=39,y=0.2,"Average=",pos = 4)
text(x=43,y=0.2,round(average_rate_Africa,3),pos = 4)



EcologicalFootprintForMiddleEastandCentralAsia <- x[x$Region=='Middle East/Central Asia',11]
PopulationForMiddleEastandCentralAsia <- x[x$Region=='Middle East/Central Asia',3]*10
NamesForMiddleEastandCentralAsia <- x[x$Region=='Middle East/Central Asia',1]
rate_of_ecologicalfootprint_populationMiddleEastandCentralAsia <- EcologicalFootprintForMiddleEastandCentralAsia/PopulationForMiddleEastandCentralAsia
average_rate_MiddleEastandCentralAsia <- sum(rate_of_ecologicalfootprint_populationMiddleEastandCentralAsia)/length(rate_of_ecologicalfootprint_populationMiddleEastandCentralAsia)
barplot(rate_of_ecologicalfootprint_populationMiddleEastandCentralAsia,names.arg = NamesForMiddleEastandCentralAsia,space = 0,col = rainbow(20),cex.names = .60,las=2,main = "Rate of Echological Footprint Graph for Middle East and Cental Asia Countries",xlab = "Countries",ylab = "RateofFootprint",cex.axis = 1)
abline(h=average_rate_MiddleEastandCentralAsia,col="black", lwd=3)
text(x=20,y=0.13,"Average=",pos = 4)
text(x=22,y=0.13,round(average_rate_MiddleEastandCentralAsia,3))



EcologicalFootprintForNorthernandEasternEurope <- x[x$Region=='Northern/Eastern Europe',11]
PopulationForNorthernandEasternEurope <- x[x$Region=='Northern/Eastern Europe',3]*10
NamesForNorthernandEasternEurope <- x[x$Region=='Northern/Eastern Europe',1]
rate_of_ecologicalfootprint_populationNorthernandEasternEurope <- EcologicalFootprintForNorthernandEasternEurope/PopulationForNorthernandEasternEurope
average_rate_NorthernandEasternEurope <- sum(rate_of_ecologicalfootprint_populationNorthernandEasternEurope)/length(rate_of_ecologicalfootprint_populationNorthernandEasternEurope)
barplot(rate_of_ecologicalfootprint_populationNorthernandEasternEurope,names.arg = NamesForNorthernandEasternEurope,space = 0,col = rainbow(20),cex.names = .60,las=2,main = "Rate of Echological Footprint Graph for Northern and Eastern Europe Countries",xlab = "Countries",ylab = "RateofFootprint",cex.axis = 1)
abline(h=average_rate_NorthernandEasternEurope,col="black", lwd=3)
text(x=9.2,y=0.13,"Average=",srt=0.2)
text(x=9.7,y=0.13,round(average_rate_NorthernandEasternEurope,3),srt=0.2)




EcologicalFootprintForLatinAmerica <- x[x$Region=='Latin America',11]
PopulationForLatinAmerica <- x[x$Region=='Latin America',3]*10
NamesForLatinAmerica <- x[x$Region=='Latin America',1]
rate_of_ecologicalfootprint_populationLatinAmerica <- EcologicalFootprintForLatinAmerica/PopulationForLatinAmerica
average_rate_LatinAmerica <- sum(rate_of_ecologicalfootprint_populationLatinAmerica)/length(rate_of_ecologicalfootprint_populationLatinAmerica)
barplot(rate_of_ecologicalfootprint_populationLatinAmerica,names.arg = NamesForLatinAmerica,space = 0,col = rainbow(20),cex.names = .60,las=2,main = "Rate of Echological Footprint Graph for Latin America Countries",xlab = "Countries",ylab = "RateofFootprint",cex.axis = 1)
abline(h=average_rate_LatinAmerica,col="black", lwd=3)
text(x=27,y=1.2,"Average=",srt=0.2)
text(x=28.4,y=1.2,round(average_rate_LatinAmerica,3),srt=0.2)



EcologicalFootprintForAsiaandPacific <- x[x$Region=='Asia-Pacific',11]
PopulationForAsiaandPacific <- x[x$Region=='Asia-Pacific',3]*10
NamesForAsiaandPacific <- x[x$Region=='Asia-Pacific',1]
rate_of_ecologicalfootprint_populationAsiaandPacific <- EcologicalFootprintForAsiaandPacific/PopulationForAsiaandPacific
average_rate_AsiaandPacific <- sum(rate_of_ecologicalfootprint_populationAsiaandPacific)/length(rate_of_ecologicalfootprint_populationAsiaandPacific)
barplot(rate_of_ecologicalfootprint_populationAsiaandPacific,names.arg = NamesForAsiaandPacific,space = 0,col = rainbow(20),cex.names = .60,las=2,main = "Rate of Echological Footprint Graph for Asian and Pasific Countries",xlab = "Countries",ylab = "RateofFootprint",cex.axis = 1)
abline(h=average_rate_AsiaandPacific,col="black", lwd=3)
text(x=26,y=0.4,"Average=",srt=0.2)
text(x=27.4,y=0.4,round(average_rate_AsiaandPacific,3),srt=0.2)






averageVector = c(average_rate_europeanUnion,average_rate_Africa,average_rate_MiddleEastandCentralAsia,average_rate_NorthernandEasternEurope,average_rate_LatinAmerica,average_rate_AsiaandPacific)
label<-c("EuropeanUnion","Africa","Middle East and Central Asia","Northern And Eastern Europe","Latin America","Asia and Pasific")

#pie chart of percentage of damage that has been given by regions

pie(averageVector,labels = label,col=rainbow(length(label)),main = "Individual's Responsibility to the Enviroment by Continental ")
legend("bottomleft",label,cex=0.35,fill=rainbow(length(averageVector)))









averageHDIofEuropeanUnion <- sum(x[x$Region=='European Union',4])/length(x[x$Region=='European Union',4])

averageHDIofAfrica <- sum(x[x$Region=='Africa',4])/length(x[x$Region=='Africa',4])

averageHDIofMiddleEastandCentralAsia <- sum(x[x$Region=='Middle East/Central Asia',4])/length(x[x$Region=='Middle East/Central Asia',4])

averageHDIofNorthernandEasternEurope <- sum(x[x$Region=='Northern/Eastern Europe',4])/length(x[x$Region=='Northern/Eastern Europe',4])

averageHDIofLatinAmerica <- sum(x[x$Region=='Latin America',4])/length(x[x$Region=='Latin America',4])

averageHDIofAsiaandPacific <- sum(x[x$Region=='Asia-Pacific',4])/length(x[x$Region=='Asia-Pacific',4])


averageHDIVector <- c(averageHDIofEuropeanUnion,averageHDIofAfrica,averageHDIofMiddleEastandCentralAsia,averageHDIofNorthernandEasternEurope,averageHDIofLatinAmerica,averageHDIofAsiaandPacific)


responsibility <- averageHDIVector*averageVector




cor.test(averageHDIVector, averageVector, method = "pearson")

plot(averageVector,averageHDIVector,main = "Scatter",las=1)

plot(averageVector,type = "l",col="blue",lwd=2)
lines(averageHDIVector,type = "l",col="red",lwd=2)
lines(responsibility,type = "l",col="green",lwd=2)
axis(1,at=1:3,c("eli"))
legend(3,0.7,legend = c("footprint","hdi","responsiblity"),col = c("blue","red","green"),lty=7,cex=0.8)


abline(a=1,b=2)

help(abline)

pairs(averageHDIVector)

############################################
