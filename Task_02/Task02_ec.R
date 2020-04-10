setwd("C:\\Users\\Sherm\\Desktop\\Evolution\\Tasks\\Task_02")
# Read in the data
beren3 <- read.csv("beren_new.csv")

#new object
naps <- which(beren3$event == "nap")
beren4 <- beren3[naps,]

head(beren4)

#times for naps
timestart <- beren4$start_minute/60
timeend <- beren4$end_minute/60
beren4$napstart <- beren4$start_hour + beren4$start_minute
beren4$napend <- beren4$end_hour + beren4$end_minute
beren4$naplength <- beren4$napend - beren4$napstart

naptime <- tapply(beren4$naplength, beren4$age, sum, na.rm=T)

#plot
par(las=1, mar=c(5, 5, 1, 1), mgp=c(2, 0.5, 0), tck=-0.01)
plot(as.numeric(names(naptime)), naptime, type="b", pch=16, xlab="day", ylab="naptime")
#correlation test
cor.test(beren4$age, naplength)
#the nature of this relationship indicates that the two variables which is the total slept time and the days show no relationship to one another


