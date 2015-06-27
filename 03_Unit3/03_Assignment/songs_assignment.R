songs = read.csv("songs.csv")
#problem 1
str(songs)
summary(songs)
table(songs$year)
table(songs$artistname)
MichaelJackson = subset(songs, artistname == "Michael Jackson")
nrow(MichaelJackson)
MichaelJackson[c("songtitle", "Top10")]
MichaelJackson[(MichaelJackson$Top10 == 1),c("songtitle", "Top10")]
levels(factor(songs$timesignature))
sort(table(songs$timesignature))
songs[which.max(songs$tempo),2]

#Problem 2
SongsTrain = subset(songs, year <= 2009)
str(SongsTrain)
SongsTest = subset (songs, year >= 2010)
str(SongsTrain)

nonvars = c("year", "songtitle", "artistname", "songID", "artistID")
SongsTrain = SongsTrain[ , !(names(SongsTrain) %in% nonvars) ]
SongsTest = SongsTest[ , !(names(SongsTest) %in% nonvars) ]
SongsLog1 = glm(Top10 ~ . , data = SongsTrain, family = binomial)
summary(SongsLog1)

#Problem 3
cor(SongsTrain$energy, SongsTrain$loudness)
SongsLog2 = glm(Top10 ~ . - loudness, data=SongsTrain, family=binomial)
summary(SongsLog2)
SongsLog3 = glm(Top10 ~ . - energy, data=SongsTrain, family=binomial)
summary(SongsLog3)

#PROBLEM4
predictTop10 = predict(SongsLog3, type = "response", newdata = SongsTest)
t = 0.45
a = table(SongsTest$Top10, predictTop10 >= t)
accuracy = (a[1] + a[4])/ sum(a)
error.rate = (a[2] + a[3]) / sum(a)
sesnitivity = a[4]/(a[2] + a[4])
specificity = a[1]/(a[1] + a[3])
false.neg.error.rate = a[2]/ (a[2] + a[4])
false.pos.error.rate = a[3]/(a[1] + a[3])

table(SongsTest$Top10)
314/(314+59)
sesnitivity
specificity
