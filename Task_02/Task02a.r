cd ~/Desktop
cd Evolution
cd Tasks
mkdir Task_02
getwd()
setwd("C:\\Users\\Sherm\\Desktop\\Evolution\\Tasks\\Task_02")
data <- read.csv("http://jonsmitchell.com/data/beren.csv", stringsAsFactors=F)
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
Feeds <- which(beren[,9] == "bottle")
write.csv(beren, "berendat.cvs", quote=F)
attach(beren)
beren <- read.csv("http://jonsmitchell.com/data/beren.csv", stringsAsFactors=F)
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
cd ~/Desktop/Evolution/Tasks
git add -A
git commit -m "Sherwin Miller Task 02 upload a"
git push -u origin master