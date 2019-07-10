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

barplot(table(m))

#european union is clearly beyond the others..


EcologicalFootprintForEuropeanUnion <- x[x$Region=='European Union',11]
PopulationForEuropeanUnion <- x[x$Region=='European Union',3]*10
NamesForEuropeanUnion <- x[x$Region=='European Union',1]
rate_of_ecologicalfootprint_population <- EcologicalFootprintForEuropeanUnion/PopulationForEuropeanUnion
barplot(rate_of_ecologicalfootprint_population,names.arg = NamesForEuropeanUnion,space = 0,col = rainbow(20),cex.names = .60,las=2)


EcologicalFootprintForAfrica <- x[x$Region=='Africa',11]
PopulationForAfrica <- x[x$Region=='Africa',3]*10
NamesForAfrica <- x[x$Region=='Africa',1]
rate_of_ecologicalfootprint_populationAfrica <- EcologicalFootprintForAfrica/PopulationForAfrica
barplot(rate_of_ecologicalfootprint_populationAfrica,names.arg = NamesForAfrica,space = 0,col = rainbow(20),cex.names = .60,las=2)


EcologicalFootprintForMiddleEastandCentralAsia <- x[x$Region=='Middle East/Central Asia',11]
PopulationForMiddleEastandCentralAsia <- x[x$Region=='Middle East/Central Asia',3]*10
NamesForMiddleEastandCentralAsia <- x[x$Region=='Middle East/Central Asia',1]
rate_of_ecologicalfootprint_populationMiddleEastandCentralAsia <- EcologicalFootprintForMiddleEastandCentralAsia/PopulationForMiddleEastandCentralAsia
barplot(rate_of_ecologicalfootprint_populationMiddleEastandCentralAsia,names.arg = NamesForMiddleEastandCentralAsia,space = 0,col = rainbow(20),cex.names = .60,las=2)


EcologicalFootprintForNorthernandEasternEurope <- x[x$Region=='Northern/Eastern Europe',11]
PopulationForNorthernandEasternEurope <- x[x$Region=='Northern/Eastern Europe',3]*10
NamesForNorthernandEasternEurope <- x[x$Region=='Northern/Eastern Europe',1]
rate_of_ecologicalfootprint_populationNorthernandEasternEurope <- EcologicalFootprintForNorthernandEasternEurope/PopulationForMiddleEastandCentralAsia
barplot(rate_of_ecologicalfootprint_populationNorthernandEasternEurope,names.arg = NamesForMiddleEastandCentralAsia,space = 0,col = rainbow(20),cex.names = .60,las=2)


EcologicalFootprintForLatinAmerica <- x[x$Region=='Latin America',11]
PopulationForLatinAmerica <- x[x$Region=='Latin America',3]*10
NamesForLatinAmerica <- x[x$Region=='Latin America',1]
rate_of_ecologicalfootprint_populationLatinAmerica <- EcologicalFootprintForLatinAmerica/PopulationForLatinAmerica
barplot(rate_of_ecologicalfootprint_populationLatinAmerica,names.arg = NamesForLatinAmerica,space = 0,col = rainbow(20),cex.names = .60,las=2)



EcologicalFootprintForAsiaandPacific <- x[x$Region=='Asia-Pacific',11]
PopulationForAsiaandPacific <- x[x$Region=='Asia-Pacific',3]*10
NamesForAsiaandPacific <- x[x$Region=='Asia-Pacific',1]
rate_of_ecologicalfootprint_populationAsiaandPacific <- EcologicalFootprintForAsiaandPacific/PopulationForAsiaandPacific
barplot(rate_of_ecologicalfootprint_populationAsiaandPacific,names.arg = NamesForAsiaandPacific,space = 0,col = rainbow(20),cex.names = .60,las=2)

average_rate_europeanUnion<-sum(rate_of_ecologicalfootprint_population)/length(rate_of_ecologicalfootprint_population)
average_rate_Africa <-sum(rate_of_ecologicalfootprint_populationAfrica)/length(rate_of_ecologicalfootprint_populationAfrica)
average_rate_MiddleEastandCentralAsia <-sum(rate_of_ecologicalfootprint_populationMiddleEastandCentralAsia)/length(rate_of_ecologicalfootprint_populationMiddleEastandCentralAsia)
average_rate_NorthernandEasternEurope<-sum(rate_of_ecologicalfootprint_populationNorthernandEasternEurope)/length(rate_of_ecologicalfootprint_populationNorthernandEasternEurope)
average_rate_LatinAmerica <-sum(rate_of_ecologicalfootprint_populationLatinAmerica)/length(rate_of_ecologicalfootprint_populationLatinAmerica)
average_rate_AsiaandPacific <-sum(rate_of_ecologicalfootprint_populationAsiaandPacific)/length(rate_of_ecologicalfootprint_populationAsiaandPacific)



cat("Average rate of Ecological footprint/population in European Union : ",average_rate_europeanUnion)


cat("Average rate of Ecological footprint/population in Africa : ",average_rate_Africa)



averageVector = c(average_rate_europeanUnion,average_rate_Africa,average_rate_MiddleEastandCentralAsia,average_rate_NorthernandEasternEurope,average_rate_LatinAmerica,average_rate_AsiaandPacific)

#pie chart of percentage of damage that has been given by regions

pie(averageVector)
  
averageHDIofEuropeanUnion <- sum(x[x$Region=='European Union',4])/length(x[x$Region=='European Union',4])

averageHDIofAfrica <- sum(x[x$Region=='Africa',4])/length(x[x$Region=='Africa',4])

averageHDIofMiddleEastandCentralAsia <- sum(x[x$Region=='Middle East/Central Asia',4])/length(x[x$Region=='Middle East/Central Asia',4])

averageHDIofNorthernandEasternEurope <- sum(x[x$Region=='Northern/Eastern Europe',4])/length(x[x$Region=='Northern/Eastern Europe',4])

averageHDIofLatinAmerica <- sum(x[x$Region=='Latin America',4])/length(x[x$Region=='Latin America',4])

averageHDIofAsiaandPacific <- sum(x[x$Region=='Asia-Pacific',4])/length(x[x$Region=='Asia-Pacific',4])


averageHDIVector <- c(averageHDIofEuropeanUnion,averageHDIofAfrica,averageHDIofMiddleEastandCentralAsia,averageHDIofNorthernandEasternEurope,averageHDIofLatinAmerica,averageHDIofAsiaandPacific)


plot(averageVector,averageHDIVector)



############################################



