#Task04

#set directory
setwd("C:\\Users\\Sherm\\Desktop\\Evolution\\Tasks\\Task_04")

#Make our populations
trueMean1 <- 5
trueSD1 <- 5
population1 <- rnorm(1e6, trueMean1, trueSD1)

trueMean2 <- 4
trueSD2 <- 5
population2 <- rnorm(1e6, trueMean2, trueSD2)

#Now take a sample of each population
Size <- 50
Sample1 <- sample(population1, Size)
Sample2 <- sample(population2, Size)

#Compare the samples! Are they different? Were the populations different? Sample 1 and Sample 2 are different and the populations are different as well. 
boxplot(Sample1, Sample2)

#Read in the needed functions
source("http://jonsmitchell.com/code/simFxn04.R")

#Make grandparents
MatGrandma <- makeFounder("grandma_mom", len=2000)
MatGrandpa <- makeFounder("grandpa_mom", len=2000)
PatGrandma <- makeFounder("grandma_da", len=2000)
PatGrandpa <- makeFounder("grandpa_da", len=2000)

#Head grandparents
head(MatGrandma)
head(MatGrandpa)
head(PatGrandma)
head(PatGrandpa)

#nrow grandparents
nrow(MatGrandma)
nrow(MatGrandpa)
nrow(PatGrandma)
nrow(PatGrandpa)

#Paternal grandparents make Alan
Alan <- makeBaby(PatGrandma, PatGrandpa)

#Maternal grandparents make Brenda
Brenda <- makeBaby(MatGrandma, MatGrandpa)

#child will be focus
Focus <- makeBaby(Brenda, Alan)

#percent of genes shared between Brenda and Focus. The percent of genes will be 50%.
ToMom <- length(grep("mom", Focus)) / length(Focus)
head(ToMom)

#how many genes Focus shared with each of maternal grandparents. Focus shares 48% with the mom's dad and 45.2% with the mom's dad.  
ToMomMom <- length(grep("grandma_mom", Focus)) / length(Focus)
ToMomDad <- length(grep("grandpa_mom", Focus)) / length(Focus)
head(ToMomMom)
head(ToMomDad)
ToDadMom <- length(grep("grandma_da", Focus)) / length(Focus)
ToDadDad <- length(grep("grandpa_da", Focus)) / length(Focus)
head(ToDadMom)
head(ToDadDad)


#Focus is not equally related to each maternal and paternal grandparent. This was not what I was expecting.

#the average relatedness of focus to all four grandparents. the average relatedness of focus to all four grandparents is 0.048.   
mean(ToMomMom, ToMomDad, ToDadMom, ToDadDad)


#Focus gets sibling
Sibling_01 <- makeBaby(Brenda, Alan)

#the amount of DNA expected Focus to share with Sibling_01.13.05% is how much DNA I would expect Focus to share with Sibling_01. This is amount actually shared.   
ToSib <- length(intersect(Focus, Sibling_01)) / length(Focus)
head(ToSib)

#the amount of genes that Focus shares with each of the 1,000 siblings.The amount is 49.3%. 
ManySiblings <- replicate(1e3, length(intersect(Focus, makeBaby(Brenda, Alan))) / length(Focus))
head(ManySiblings)

#summarize data
quantile(ManySiblings)
mean(ManySiblings)

#plot data
plot(density(ManySiblings), main="", xlab="proportion shared genes")

#The reason you see a range of values in these analyses is because there are different percentages from grandparents, parents, and siblings related to Focus.  

#calculate expected genotype frequencies
HWE <- function(p) {
  aa <- p^2
  ab <- 2 * p * (1 - p)
  bb <- (1 - p)^2
  return(c(aa=aa, ab=ab, bb=bb))
}
HWE(0.5)

#make blank plot
plot(1, 1, type="n", xlim=c(0, 1), ylim=c(0, 1), xlab="freq. allele a", ylab="geno. freq")

#calculare genotypic frequencies for a bunch of allele frequencies
p <- seq(from = 0, to = 1, by = 0.01)
GenoFreq <- t(sapply(p, HWE))

#plot known allele frequency against expected genotypic frequencies
lines(p, GenoFreq[, "aa"], lwd=2, col="red")

#as the frequency of a allele increases, so does the frequency of aa individuals. if the frequency of the a allele decreases, so does the frequency of aa individuals.

#add other genotypes
lines(p, GenoFreq[, "ab"], lwd=2, col="purple")
lines(p, GenoFreq[, "bb"], lwd=2, col="blue")
legend("top", legend=c("aa", "ab", "bb"), col=c("red", "purple", "blue"), lty=1, lwd=2, bty="n")

#simulate a population
Pop <- simPop(500)

#add points to HWE plot
points(Pop[,"freqa"], Pop[,"Genotypes.aa"]/500, pch=21, bg="red")

#Yes, the frequency of the aa genotype in my population match the expectation from Hardy-Weinberg.

#another simulation with smaller population
Pop <-simPop(50)
points(Pop[, "freqa"], Pop[, "Genotypes.aa"]/50, pch=22, bg="red")

#The frequency of the aa genotype in my population changed and it does not match Hardy Weinberg equilibrium.

#install the LearnPopGen package
install.packages("learnPopGen")
library(learnPopGen)

#watch lines
x <- genetic.drift(Ne=200, nrep=5, pause=0.01)

#change Ne
x2 <- genetic.drift(Ne=100, nrep=5, pause=0.01)

#look at how population sizes effect the time to extinction for one allele

#make a bunch of populations of different sizes from 5 to 50 individuals
PopSizes <- 5:50

#there are 5 populations with each given size
Samples <- rep(PopSizes, 5)

#simulate all 230 of those populations and get the time one of the two alleles went extinct
tExt <- sapply(Samples, function(x) nrow(simPop(x, 500)))

#fit linear model data into R
Line <- lm(tExt ~ Samples)

#see fit
summary(Line)

#extract coefficients
Line$coef

#to add it to the plot
plot(Samples, tExt)
abline(Line)

#line 2
Line2 <- lm(tExt~Samples +0)

#see fit
summary(Line2)

#extract coefficients
Line2$coef

#add it to the plot
plot(Samples, tExt)
abline(Line2)

#+0 is the y intercept and the time for extinction. 

#Based on the graph, as the population size increases, genetic drift gets slower and it takes a long time for alleles to go extinct.  

install.packages("lmtest")
library(lmtest)
bptest(Line)

install.packages("sandwich")
library(sandwich)
vcovHC(Line)
coeftest(Line, vcov = vcovHC(Line))
coeftest(Line, vcov = vcovHC(Line, type = "HC0"))

install.packages("robustbase")
library(robustbase)
Linerobfit <- lmrob(tExt~Samples)
summary(Linerobfit)

