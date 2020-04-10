#Task 06

setwd("C:\\Users\\Sherm\\Desktop\\Evolution\\Tasks\\Task_06")

#install learnPopGen

# two loci in a set of 10 individuals. 
par(mfrow=c(1,2))
coalescent.plot()
axis(1, at=c(1:10), labels=letters[1:10])
mtext(side=3, text="Wnt4")
coalescent.plot()
mtext(side=3, text="Tbx5")
axis(1, at=c(1:10), labels=letters[1:10])



install.packages("learnPopGen")
library("learnPopGen")

#install coala
install.packages("coala")
library("coala")

#install phytools
install.packages("phytools")
library("phytools")

#learnPopGen
?coalescent.plot()

#coalescentplot1
coalescent.plot(n=5, ngen=20, col.order="alternating")

#save coalescentplot1
pdf("r06-smallcoalescentplot.pdf", height = 4, width = 4)
coalescent.plot(n=5, ngen=20, col.order="alternating")
dev.off()

#coalescentplot2
coalescent.plot(n=20, ngen=30, col.order="alternating")

#savecoalescentplot2
pdf("r06-mediumcoalescentplot.pdf", height = 4, width = 4)
coalescent.plot(n=20, ngen=30, col.order="alternating")
dev.off()


#coalescentplot3
coalescent.plot(n=50, ngen=40, col.order="alternating")

#savecoalescentplot 3
pdf("r06-largecoalescentplot.pdf", height = 4, width = 4)
coalescent.plot(n=50, ngen=40, col.order="alternating")
dev.off()

#question 1. It is impossible to interpret the number of alleles and modify the number of alleles. the simulation runs random with the number of alleles. 

#question 2. It takes approximately 10 to 20 generations for one allele to go to fixation. 

#question 3. Thw average number of offspring each haploid individual has is one and the variance is one.  

#question 4. Fitness does not play a role in these simulations. instead, genetic drift plays a role in these simulations because it is random.  

#question 5. No, the recent common ancestor for the focal locus is not typically always alive in generation zero.  

library(phytools)
library(coala)

#set up model
model <- coal_model(sample_size = 5, loci_number = 10, loci_length = 500, ploidy = 2)+ feat_mutation(10) + feat_recombination(10) + sumstat_trees() + sumstat_nucleotide_div()

#run simulation
stats <- simulate(model, nsim = 1)

#measure of genetic diversity
Diversity <- stats$pi

#Looking at diversity all the numbers are not the same. These differences could be due to genetic drift. 
head(Diversity)

#Each SNP in each locus has its own ancestry tree
Nloci <- length(stats$trees)


#look at the first SNP for the first locus
t1 <- read.tree(text=stats$trees[[1]][1])
plot(t1)
axisPhylo()

#Question 6. The reason why is because the tips refer to the different chromosome sets from the five individuals.  

#find age of the most recent ancestor for this SNP on this locus for these individuals
Age1 <- max(nodeHeights(t1))
head(Age1)

#look at first SNP of the second locus
t2 <- read.tree(text=stats$trees[[2]][1])
plot(t2)
axisPhylo()

#age of most recent common ancestor for second SNP. The second SNP is present compared to the first SNP. 
Age2 <- max(nodeHeights(t2))
head(Age2)

#Question 7.No, the first and second SNP do not match.  
par(mfrow=c(1,3))
plot(t1)
axisPhylo()
plot(t2)
axisPhylo()

#compare the trees
compare.chronograms(t1, t2)

#make more comparisons
t1_1 <- read.tree(text=stats$trees[[1]][1])
t1_2 <- read.tree(text=stats$trees[[1]][2])
compare.chronograms(t1_1, t1_2)

#compare all of the SNPs from all of the loci all at once
for (locus in 1:Nloci) {
  ntrees <- length(stats$trees[[locus]])
  for (n in 1:ntrees) {
    if (locus == 1 && n == 1) {
      outPhy <- read.tree(text=stats$trees[[locus]][n])
      }
    else {
      outPhy <- ape:::c.phylo(outPhy, read.tree(text=stats$trees[[locus]][n]))
    }
     }
  }

#plot trees all at once
par(mfrow=c(1,1))
densityTree(outPhy)

#study opportunity, change model
model2 <- coal_model(sample_size = 5, loci_number = 10, loci_length = 500, ploidy = 2) + feat_mutation(5) + feat_recombination(10) + sumstat_trees() + sumstat_nucleotide_div()

#run simulation 2
stats <- simulate(model2, nsim = 1)

#Diversity 2
Diversity2 <- stats$pi

head(Diversity)

#Each SNP in each locus has its own ancestry tree
Nloci <- length(stats$trees)

#look at the first SNP for the first locus
t1 <- read.tree(text=stats$trees[[1]][1])
plot(t1)
axisPhylo()

#find age of the most recent ancestor for this SNP on this locus for these individuals
Age1 <- max(nodeHeights(t1))
head(Age1)

#look at first SNP of the second locus
t2 <- read.tree(text=stats$trees[[2]][1])
plot(t2)
axisPhylo()

#age of most recent common ancestor for second SNP. The second SNP is present compared to the first SNP. 
Age2 <- max(nodeHeights(t2))
head(Age2)

#plot t1 and t2  
par(mfrow=c(1,2))
plot(t1)
axisPhylo()
plot(t2)
axisPhylo()

#compare the trees
compare.chronograms(t1, t2)

#make more comparisons
t1_1 <- read.tree(text=stats$trees[[1]][1])
t1_2 <- read.tree(text=stats$trees[[1]][2])
compare.chronograms(t1_1, t1_2)

#compare all of the SNPs from all of the loci all at once
for (locus in 1:Nloci) {
  ntrees <- length(stats$trees[[locus]])
  for (n in 1:ntrees) {
    if (locus == 1 && n == 1) {
      outPhy <- read.tree(text=stats$trees[[locus]][n])
    }
    else {
      outPhy <- ape:::c.phylo(outPhy, read.tree(text=stats$trees[[locus]][n]))
    }
  }
}

#plot trees all at once
par(mfrow=c(1,1))
densityTree(outPhy)

#predict on model 2. I predict that model 2 has less diversity than model 1

#model 2, No, it is not different from what I predicted because I changed the mutation rate of model 2 and it shows that it has less diversity than model 1. 

#mutation rate varies in each of 40 simulations
model3 <- coal_model(10, 50) + feat_mutation(par_prior("theta", sample.int(100, 1))) + sumstat_nucleotide_div()
stats <- simulate(model3, nsim = 40)

mean_pi <- sapply(stats, function(x) mean(x$pi))
theta <- sapply(stats, function(x) x$pars[["theta"]])

#plot mean_pi against theta with regression line
Line <- lm(mean_pi ~ theta)
plot(mean_pi, theta)
abline(Line)

















  



