#Task04 extra credit

#set directory
setwd("C:\\Users\\Sherm\\Desktop\\Evolution\\Tasks\\Task_04")
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

#make a bunch of populations of different sizes from 5 to 50   individuals
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