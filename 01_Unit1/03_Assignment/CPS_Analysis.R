CPS = read.csv("CPSData.csv")
str(CPS)
summary(CPS)
CountryMap = read.csv("CountryCodes.csv")
MetroAreaMap = read.csv("MetroAreaCodes.csv")

CPS = merge(CPS, MetroAreaMap, by.x="MetroAreaCode", by.y="Code", all.x=TRUE)
CPS = merge(CPS, CountryMap, by.x="CountryOfBirthCode", by.y="Code", all.x=TRUE)


#Problem1
str(CPS)
Summary(CPS)
names(CPS)
sort(table(CPS$State))
which.min(table(CPS$State))
which.max(table(CPS$State))
table(CPS$Citizenship)/sum(table(CPS$Citizenship))
1- (table(CPS$Citizenship)/sum(table(CPS$Citizenship)))
table(CPS$Race,CPS$Hispanic) > 250
#Problem2
summary(CPS)
table(CPS$Region, is.na(CPS$Married))
table(CPS$Sex, is.na(CPS$Married))
table(CPS$Age, is.na(CPS$Married))
table(CPS$State, is.na(CPS$MetroArea))
table(CPS$Region, is.na(CPS$MetroArea))
sort(tapply(is.na(CPS$MetroArea),CPS$State,mean))

#Problem 3
str(MetroAreaMap)
str(CountryMap)
sort(table(CPS$MetroArea))
sort(tapply(CPS$Hispanic,CPS$MetroArea,mean))
tapply(CPS$Race=="Asian",CPS$MetroArea,mean) > 0.2
sort(tapply(CPS$Education == "No high school diploma", CPS$MetroArea, mean, na.rm= T))

#Problem4
summary(CPS)
str(CPS)
table(CPS$MetroArea=="New York-Northern New Jersey-Long Island, NY-NJ-PA" , CPS$Country != "United States")
sort(tapply(CPS$Country=="India",CPS$MetroArea,sum,na.rm = T))
sort(tapply(CPS$Country=="Brazil",CPS$MetroArea,sum,na.rm = T))
sort(tapply(CPS$Country=="Somalia",CPS$MetroArea,sum,na.rm = T))
