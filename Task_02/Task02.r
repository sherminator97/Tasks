#Task 02a
getwd()
 
setwd("C:\\Users\\Sherm\\Desktop\\Evolution\\Tasks\\Task_02")
data <- read.csv("http://jonsmitchell.com/data/beren.csv", stringsAsFactors=F)
write.csv(data, "raw_data.csv", quote=F)

data
length(data)
nrow(data)
ncol(data)
colnames(data)
head(data)
data[1,]
data[2,]
data[1:3,]
data[1:3, 4]
data[1:5, 1:3]
data[257,]

beren <- data
Feeds <- which(beren[,9] == "bottle")
berenMilk <- beren[Feeds,]
head(berenMilk)
Feeds <- which(beren[, "event"] == "bottle")
Feeds <- which(beren$event == "bottle")
berenMilk <- beren[Feeds,]
dayID <- apply(beren, 1, function(x) paste(x[1:3], collapse="-"))
dateID <- sapply(dayID, as.Date, format = "%Y-%m-%d", origin = " 2019-04-18")
beren$age <- dateID -dateID[which(beren$event == "birth")]
head(beren)
beren2 <- beren
beren3 <- beren2[order(beren2$age) ,]
head(beren)
head(beren2)
head(beren3)
write.csv(beren3, "beren_new.csv", quote=F, row.names=FALSE)
#Task02b
#Question 1
#the only reason why the first two hypotheses are inappropriate is because the first hypothesis doesn't have a time and the second hypothesis is the amount of naps is not related to how much Beren drinks
# Read in the data
setwd("C:\\Users\\Sherm\\Desktop\\Evolution\\Tasks\\Task_02")
beren3 <- read.csv("beren_new.csv")
Feeds <- which(beren3$event == "bottle")

# Summarize the data
avgMilk <- mean(beren3$value[Feeds])
#the units for the milk value is oz. 
#you use the value column to calculate your values from events
#the brackets can access subsets of the object and also pulled out data from beren 3

avgFeed <- tapply(beren3$value[Feeds], beren3$age[Feeds], mean)
varFeed <- tapply(beren3$value[Feeds], beren3$age[Feeds], var)
totalFeed <- tapply(beren3$value[Feeds], beren3$age[Feeds], sum)
numFeeds <- tapply(beren3$value[Feeds], beren3$age[Feeds], length)

# Correlation test on the data
cor(beren3$value[Feeds], beren3$age[Feeds])
cor.test(beren3$value[Feeds], beren3$age[Feeds])
berenCor <- cor.test(beren3$value[Feeds], beren3$age[Feeds])
summary(berenCor)

#statistical test
berenANOVA <- aov(beren3$value[Feeds] ~ beren3$caregiver[Feeds])
berenANOVA

#plots
boxplot(beren3$value[Feeds] ~ beren3$caregiver[Feeds], xlab= "who gave the bottle", ylab= "amount of milk consumed (oz)")

#las is the style of the axis labels, marc is a numerical vector of the form c(bottom, left, top, right), mgp refers to the margin line for the axis title, axis labels, and axis line, and tck is teh length of tick marks as a fraction of the smaller of the width or height of the plotting region 
?par
par(las=1, mar=c(5, 5, 1, 1), mgp=c(2, 0.5, 0), tck=-0.01)

plot(as.numeric(names(totalFeed)), totalFeed, type="b", pch=16, xlab="age in days", ylab="ounces of milk")
abline(h=mean(totalFeed), lty=2, col="red")

#save graph as PDF
pdf("r02b-totalMilkByDay.pdf", height = 4, width = 4)
par(las=1, mar=c(5, 5, 1, 1), mgp=c(2, 0.5, 0), tck=-0.01)
plot(as.numeric(names(totalFeed)), totalFeed, type="b", pch=16, xlab="age in days", ylab="ounces of milk")
abline(h=mean(totalFeed), lty =2, col="red") 
dev.off()

#jonsmitchellgraph
source("http://jonsmitchell.com/code/plotFxn02b.R")

pdf("r02b-cumulativeMilkByTime.pdf")
source("http://jonsmitchell.com/code/plotFxn02b.R")
dev.off()

#Question2
The reason the graph is impossible to interpret is because there might have been some inconsistent data.

#selfquiz
unique(beren3$event)

#Task02c

#hypothesis is the amount of solid food increases by age
setwd("C:\\Users\\Sherm\\Desktop\\Evolution\\Tasks\\Task_02")
beren <- read.csv("beren_new.csv", stringsAsFactors = FALSE)
head(beren)
Feeds <- which(beren$event == "solids")
totalFeed <- tapply(beren$value[Feeds], beren$age[Feeds], sum)
par(las=1, mar=c(5, 5, 1, 1), mgp=c(2, 0.5, 0), tck=-0.01)
plot(as.numeric(names(totalFeed)), totalFeed, type="b", pch=16, xlab="age", ylab="totalSolids")
cor.test(beren$value[Feeds], beren$age[Feeds])

#savepdf
pdf("r02c-totalSolidsByAge.pdf", height = 4, width = 4)
par(las=1, mar=c(5,5,1,1), mgp=c(2, 0.5, 0), tck= -0.01)
plot(as.numeric(names(totalFeed)), totalFeed, type="b", pch=16, xlab="age", ylab="totalSolids")
dev.off()

#extracredit
#my specific prediction is that Beren ate a total amount of 4 solid foods next Tuesday
beren <- read.csv("beren_new.csv", stringsAsFactors = FALSE)
head(beren)
Feeds <- which(beren$event == "solids")
totalFeeds <- beren[Feeds,]
head(totalFeeds)


# 1st: us lm() or some other function to model total solids by age
model <- lm(totalFeed ~ as.numeric(names(totalFeed)))

model$coefficients

300 * model$coefficients[2] + model$coefficients[1]
predict(model, newdata=data.frame(age=300))


  












