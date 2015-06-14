mvt = read.csv("mvtweek1.csv")
#Problem 1
str(mvt)
max(mvt$ID)
min(mvt$Beat) 
table(mvt$Arrest)
table(mvt$LocationDescription)
summary(mvt$LocationDescription)
#Problem 2
mvt$Date[1]
DateConvert = as.Date(strptime(mvt$Date, "%m/%d/%y %H:%M"))
summary(DateConvert)
mvt$Month = months(DateConvert)
mvt$Weekday = weekdays(DateConvert)
mvt$Date = DateConvert
which.min(table(mvt$Month))
table(mvt$Month,mvt$Arrest)
#Problem3
hist(mvt$Date, breaks=100)
boxplot(mvt$Date ~ mvt$Arrest)
table(mvt$Year, mvt$Arrest)
#Problem4
sort(table(mvt$LocationDescription),decreasing = T) [1:6]
top5name = c("PARKING LOT/GARAGE(NON.RESID.)", "STREET","ALLEY","DRIVEWAY - RESIDENTIAL","GAS STATION")
Top5 = subset(mvt, LocationDescription == top5name[1] | LocationDescription == top5name[2] | 
                      LocationDescription == top5name[3] | LocationDescription == top5name[4] |
                      LocationDescription == top5name[5])
Top5 = subset(mvt, LocationDescription %in% top5name)
str(Top5)
Top5$LocationDescription = factor(Top5$LocationDescription)
table(Top5$LocationDescription,Top5$Arrest)
table(Top5$LocationDescription,Top5$Weekday)