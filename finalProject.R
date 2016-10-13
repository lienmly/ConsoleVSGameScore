## Lien Ly - Final Project 
## Video Games Ratings Vs. Game Platforms 

## (1) ## 
## Datasource and definitions explained 
dat <- read.csv("gamedata.csv", header=TRUE)
summary(dat)
observationNum <- nrow(dat)

## Sorted mean score by platforms 
platformScore <- setNames(aggregate(dat[,3], list(dat$Platform), mean), c("Platforms", "MeanScore"))
platformScore_sorted <- platformScore[with(platformScore, order(MeanScore)), ]
colfunc <- colorRampPalette(c("cadetblue1", "cadetblue4"))
par(bg="white", cex.lab=1)
barplot(platformScore_sorted$MeanScore, main="Mean Ratings by Platforms", names.arg = platformScore_sorted$Platforms, ylab = "Rating", las = 2, col = colfunc(56), ylim = c(0,10),cex.names=0.6)

## Sorted mean score by genres 
genreScore <- setNames(aggregate(dat[,3], list(dat$Genre), mean), c("Genres", "MeanScore"))
genreScore_sorted <- genreScore[with(genreScore, order(MeanScore)), ]
colfunc_genre <- colorRampPalette(c("maroon1", "maroon4"))
par(bg="white", cex.lab=1)
barplot(genreScore_sorted$MeanScore, main = "Mean Ratings by Genres", names.arg = genreScore_sorted$Genres, ylab = "Rating", las = 2, col = colfunc_genre(96), ylim = c(0,10),cex.names=0.6)

## Frequency of scores given by IGN 
colfunc <- colorRampPalette(c("olivedrab", "olivedrab1", "olivedrab"))
hist(dat$Score, main="Score Frequency Given by IGN", xlab = "Scores", col = colfunc(19))

## (2) ##
## Main features of data set presented with visualization 

## Read data for platform types categorization 
platformDat <- read.csv("platformcategory.csv", header = TRUE)
platformDat <- platformDat[with(platformDat, order(Platform.type)),]
summary(platformDat)

## Create main data that is used for research question 
## Drop rows that have value "DVD / HD Video Game" and "Wireless 
tempDat <- dat[!dat$Platform == "DVD / HD Video Game", ]
tempDat <- dat[!dat$Platform == "Wireless", ]

## Categorize all platforms in tempData based on platformDat
tempDat <- merge(tempDat, platformDat, by.x = "Platform", by.y = "Platform")
tempDat <- tempDat[ ,!(names(tempDat) %in% c("MeanScore"))]

## Add "Console" column which has binary data (0,1) to tempDat 
tempDat$Console <- 0

## Extract all cross platform games and delete them 
tempDat <- tempDat[!(duplicated(tempDat$Game) | duplicated(tempDat$Game, fromLast = TRUE)), ]

## Set "Console"'s binary value based on "Platform.type"
tempDat$Console[tempDat$Platform.type == "Home console"] <- 1
tempDat$Console[tempDat$Platform.type == "Handheld"] <- 0

## Create "mainDat" 
drops <- c("Game", "Genre", "Platform", "Platform.type")
mainDat <- tempDat[ ,!(names(tempDat) %in% drops)]
summary(mainDat)

## mainDat visualization
plot(mainDat,xlab="IGN Score", main = "Scatterplot of Console vs. IGN Score", pch=22, yaxt="n", col=c("palevioletred2"), cex=3, lty="solid", lwd=3)
ticks <- c(0,1)
axis(2,at=ticks,labels=ticks)

## (6) ## 
## Method applied and interpretation 
mainDat.glm <- glm(Console~Score,data = mainDat, family = binomial)
summary(mainDat.glm)

## Generate tables of estimated probability and odds based on regression equation 
oddstbl <- as.data.frame(matrix(0, ncol = 1, nrow = 101))
colnames(oddstbl) <- c("IGNScore")
oddstbl$IGNScore <- seq(0,10,0.1)
## Probability a score being a console game 
oddstbl$prob_p1 <- with(oddstbl, exp(0.03967-0.04372*oddstbl$IGNScore)/(1+exp(0.03967-0.04372*oddstbl$IGNScore)))
## Probability a score being a non-console game 
oddstbl$prob_p0 <- with(oddstbl, 1 - oddstbl$prob_p)
## Odds of a score being a console game 
oddstbl$odds <- with(oddstbl, oddstbl$prob_p1/oddstbl$prob_p0)

## Create table for odds ratio match with Score increase interval 
intervalScore <- as.data.frame(matrix(0, ncol = 1, nrow = 101))
colnames(intervalScore) <- c("Score.increase")
intervalScore$Score.increase <- seq(0,10, 0.1)
## Odds ratio accordingly 
intervalScore$oddsRatio <- with(intervalScore, oddstbl$odds[intervalScore$Score.increase == oddstbl$IGNScore]/1.0404674)
## Percent decrease in odds 
intervalScore$percentDec <- with(intervalScore, (1-intervalScore$oddsRatio)*100)
## Plot this graph to get exponential regression line 
plot(intervalScore$Score.increase, intervalScore$oddsRatio, xlab = "Score Intervals (Delta)", ylab = "Odds Ratio", type = "l", main = "Score Delta and Odds Ratio")
## The graph above looks the same as: 
x <- seq(1, 10)
plot(x, exp(-0.04372*x), xlab = "x-values", ylab = "f(x)", type = "l", main = "y = e^(-0.04372)x")

## Generate confidence intervals for the regression coefficients 
confint(mainDat.glm)

## Put the coefficients and confidence intervals onto a useful scale 
exp(mainDat.glm$coefficients)
exp(confint(mainDat.glm))

## plot(mainDat$Score, fitted.values(mainDat.glm))
