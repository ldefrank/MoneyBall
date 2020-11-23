batting <- read.csv("Batting.csv")
head(batting)
attach(batting)
head(AB,5)
head(X2B,5)
BA = H/AB
tail(BA,5)

OBP = (H + BB + HBP)/(AB + BB + HBP + SF)
X1B = H - X2B - X3B - HR
SLG = ((X1B) + (2 * X2B) + (3 * X3B) + (4 * HR)) / AB

batting1 <- cbind(batting, BA, OBP, X1B, SLG)
str(batting1)

salaries <- read.csv("Salaries.csv")
summary(salaries)

batting2 <- subset(batting1, yearID > 1984)
summary(batting2)

combo <- merge(batting2, salaries, c('playerID', 'yearID'))
summary(combo)

lost_players <- subset(combo, playerID %in% c('giambja01','damonjo01','saenzol01' ))
summary(lost_players)

lost_players <- subset(lost_players, yearID == 2001)
dim(lost_players)

lost_players <- lost_players[, c('playerID', 'H', 'X2B', 'X3B', 'HR', 'OBP', 'BA', 'SLG', 'AB')]
head(lost_players)

install.packages("dplyr")
library(dplyr)

replacements <- subset(combo, yearID == '2001')

library(ggplot2)
ggplot(replacements, aes(x=OBP, y=salary)) + geom_point()

replacements1 <- filter(replacements, salary < 7000000, OBP > 0)
ggplot(replacements1, aes(x=OBP, y=salary)) + geom_point()

sum(lost_players$AB)
1469/3
replacements2 <- filter(replacements1, AB >= 489.6667)
ggplot(replacements2, aes(x=OBP, y=salary)) + geom_point()

possible <- head(arrange(replacements2, desc(OBP)), 10)
possible1 <- possible[, c('playerID', 'OBP', 'AB', 'salary')]
possible1 