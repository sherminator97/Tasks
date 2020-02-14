#Task 05

#get directory
getwd()

#source
source("http://jonsmitchell.com/code/fxn05.R")

#run a simple population
Pop1 <- simPop(Popsize = 50, nGenerations = 100, initial_p = 0.5, h = 1, s = 0)
plot(1:nrow(Pop1), Pop1[,1], ylim=c(0, 1), type = "l", xlab = "generation", ylab = "allele freq.", lwd=2)
lines(1:nrow(Pop1), Pop1[,2], lwd=2, col='red')
legend("topleft", legend = c("a", "b"), col = c("black", "red"), lwd=2, bty="n")

#plot relative fitnesses of genotypes vs the frequencies of allele a
plotFit(nruns = 10, n = 50, ngens = 100, init_p = 0.5, h = 1, s = 0)

#expect to see four equal categories
Expectation <- c(10, 10, 10, 10)

#pretend what we actually see is four uneven categories
Observed <- c(15, 15, 5, 5)

#calculate Chi squared statistic
Chisq <- sum(((Expectation - Observed) ^2) / Expectation)

#visualize what this means
barplot(rbind(Expectation, Observed), beside =T, main = bquote(chi^2 ~ "=" ~. (Chisq)), legend.text=c("expected", "observed"))

#Observed 2
Observed2 <- c(5, 0, 0, 35)

#calculate Chi squared statistic
Chisq <- sum(((Expectation - Observed2) ^2) / Expectation)

#visualize what this means
barplot(rbind(Expectation, Observed2), beside =T, main = bquote(chi^2 ~ "=" ~. (Chisq)), legend.text=c("expected", "observed"))

#Observed 3
Observed3 <- c(2, 3, 10, 30)

#calculate Chi squared statistic
Chisq <- sum(((Expectation - Observed3) ^2) / Expectation)

#visualize what this means
barplot(rbind(Expectation, Observed3), beside =T, main = bquote(chi^2 ~ "=" ~. (Chisq)), legend.text=c("expected", "observed"))

#Observed 4
Observed4 <- c(10, 10, 10, 10)

#calculate Chi squared statistic
Chisq <- sum(((Expectation - Observed4) ^2) / Expectation)

#visualize what this means
barplot(rbind(Expectation, Observed4), beside =T, main = bquote(chi^2 ~ "=" ~. (Chisq)), legend.text=c("expected", "observed"))

#observed 5
Observed5 <- c(40, 0, 0, 0)

#calculate chi square statistic
Chisq <- sum(((Expectation - Observed5) ^2) / Expectation)

#visualize what this means
barplot(rbind(Expectation, Observed5), beside =T, main = bquote(chi^2 ~ "=" ~. (Chisq)), legend.text=c("expected", "observed"))
             


#The chi square value when you observe all 10s is 0. When all 40 observations are only in one category, the observed in that category only shows up as compared to the other categories.  The chi square relates to the eveness of the bars by the chi square sum in each category. 

#set working directory
setwd("C:\\Users\\Sherm\\Desktop\\Evolution\\Tasks\\Task_05")

#read data
results <- read.csv("http://jonsmitchell.com/data/biol112labresults.csv", stringsAsFactors=F)

#subset data
counts <- results[, c("yellow", "red", "green", "blue", "black", "tan")]

#list out background colors
backgrounds <- c("White", "Red", "Yellow", "Green", "Blue", "Black")

#slightly nicer colors
backgroundCol <- c("white", "#d53e4f", "#fee08b", "#abdda4", "#3288bd", "black")

#calculate Chi square statistic for the first row of counts
calcChi(counts[1,])

#calculate Chi squared for all of the rows at once
Chisqs <- apply(counts, 1, calcChi)

#visualize Chi square
plotChis(counts)

#the bars are not even when chi square is high, but the bars are even when Chi square is low. plotChis() shows the number of counts for each color to interpret the number.  

#average chisquared
Avg <- mean(Chisqs)
head(Avg)

#The average Chi squared is 60.99. I would just look at the color that has the most counts.

#The average Chi square value is greater than the critical value. This could mean that natural selecction occured. 

#Does the chi squared differ by background? Yes, the chisquare differs by background
backgroundAvgs <- tapply(Chisqs, results[,3], mean)
head(backgroundAvgs)

#how many chi squareds show a clear difference?
propSig <- length(which(Chisqs > 11.70)) / length(Chisqs)
percSig <- round(100 * propSig)

#This number surprised me
head(percSig)

#Another thing that can drive the high number other than natural selection could be genetic drift.

#plot of Chi square results
par(las= 1, mar = c(4, 4, 1, 1), mgp = c(2, 0.5, 0), tck = -0.01, cex.axis=1)
hist(Chisqs, main="", xlab="chi-squared values", ylab="frequency")

#background specific patterns, start with empty plot
par(las= 1, mar = c(4, 4, 1, 1), mgp = c(2, 0.5, 0), tck = -0.01, cex.axis=1)
plot(1, 1, xlim=c(0, 400), ylim=c(1, 8.5), xlab="", ylab="", type="n", yaxt="n")

#set up y-axis and x-axis labels
axis(2, at = 1:length(backgrounds), labels = backgrounds)
mtext(side=1, expression(chi^2), cex=1.75, line=2.5)

#add histogram of the data
counter <- 1
for(i in backgrounds) {
  Data <- Chisqs[which(results[,3] == i)]
  addHist(Y=counter, Dat=Data, Color=backgroundCol[counter])
  counter <- counter +1
}

#add line representing critical value
abline( v = 11.70, lty=2, lwd=2, col='black')

#Yes, for example, yellow is more significant than blue because there is more stuff on the right. 

#simulate running this experiment without natural selection ten thousand times
Simulation <- simDraws(10000)

#add Chisquareds to plot
addHist(Y=7, Dat=Simulation, Color="lightgray")
mtext(side=2, at=7, line=0, "simulated")

#compare it to the line
abline(v = 11.70, lty=2, lwd=2)

#The percentage of time that the selection-free simulation finds a significant (greater than 11.70) result is around 50%. 
#The reason that the selection free simulation produces final counts so different than initial counts is because this is happening at random. 

#Fit
Fit <- c(1, 1, 1, 1, 1, 1)
names(Fit) <- 1:6
Simulation2 <- simDraws(1e4, w = Fit)
addHist(Y=8, Dat=Simulation2, Color=rgb(0, 0, 0, 0.25))

#one tooth pick type selected against
Fit <- c(0.1, 1, 1, 1, 1, 1)
names(Fit) <- 1:6
Simulation3 <- simDraws(1e4, w = Fit)
addHist(Y=8, Dat=Simulation3, Color=rgb(0, 0, 0, 0.25))

#three tooth pick types selected against
Fit <- c(0.5, 0.6, 0.7, 1, 1, 1)
names(Fit) <- 1:6
Simulation4 <- simDraws(1e4, w = Fit)
addHist(Y=8, Dat=Simulation4, Color=rgb(0, 0, 0, 0.25))

#five selected against
Fit <- c(0.1, 0.2, 0.3, 0.4, 0.5, 1)
names(Fit) <- 1:6
Simulation5 <- simDraws(1e4, w = Fit)
addHist(Y=8, Dat=Simulation5, Color=rgb(0, 0, 0, 0.25))

#insane selection
Fit <- c(0.1, 0.1, 0.1, 0.1, 0.1, 1)
names(Fit) <- 1:6
Simulation6 <- simDraws(1e4, w = Fit)
addHist(Y=8, Dat=Simulation6, Color=rgb(0, 0, 0, 0.25))
mtext(side=2, at=8, line=0, "sel. sim.")

#plot mixture of simulations
Simulation7 <- c(Simulation2, Simulation3, Simulation4, Simulation5, Simulation6)
addHist(Y=8, Dat=Simulation7, Color=rgb(0, 0, 1, 1))

#The mixture is similarily the same as the student generated data. I would say that the student groups showed strong evidence of selection and mutation. 

#I would say that the relative strength of different evolutionary processes across all the groups in the labs across all the different years is that mutation and selection are strong. 

#The evolutionary processes that are at work in the lab as done by humans are mutation and natural selection. 

#The evolutionary processes that are at work in the lab as simulated by the computer are genetic drift, natural selection and mutation. 

#What these graphs tell us about the relative strength of the evolutionary processes the BIOL-112 students are simulating is that natural selection and mutation are consistent. 

#The data tells us that the processes are occurring are uneven. 

#Mutation will increase the chi square value because the toothpick will mutate into a different type.









