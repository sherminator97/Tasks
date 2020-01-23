setwd("C:\\Users\\Sherm\\Desktop\\Evolution\\Tasks\\Task_02")
# Read in the data
beren3 <- read.csv("beren_new.csv")

#new object
naps <- which(beren3$event == "nap")
beren4 <- beren3[naps,]

head(beren4)

#times for each nap
timestart <- beren4$start_hour + beren4$start_minute/60
timeend <- beren4$end_hour + beren4$end_minute/60

#totaltimespent sleeping
duration <- timeend - timestart
duration [1:6]
timestart[1:6]
timeend[1:6]
#total sum
totaltime <-tapply(beren4$day[naps], beren4$age[naps], sum)
?par

#plot
par(las=1, mar=c(5, 5, 1, 1), mgp=c(2, 0.5, 0), tck=-0.01)
plot(as.numeric(names(totaltime)), totaltime, type="b", pch=16, xlab="day", ylab="totaltime")
#correlation test
cor.test(beren4$age, duration)
#the nature of this relationship indicates that the two variables which is the total slept time and the days show no relationship to one another


