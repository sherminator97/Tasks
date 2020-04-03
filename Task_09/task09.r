#set working directory
setwd("C:\\Users\\Sherm\\Desktop\\Evolution\\Tasks\\Task_09")

#functions from phytools library
library(phytools)

#read phylogeny into R
tree <- read.tree("http://www.phytools.org//Cordoba2017//data//Anolis.tre")
plot(tree, type="fan")

#question 1. there are 100 tips and branch lengths are present. there is a total of 198 branch lengths.  
tiplabels(frame="circle", bg='lightblue', cex=1)
tree$tip.label
head(tree)
edgelabels(tree$edge.length, bg="black", col="white", font=2)

#read data
data <- read.csv("https://jonsmitchell.com/data/svl.csv", stringsAsFactors=F, row.names=1)

#question2. Data is an object that contains species with the value of its snout-vent length.
#the dimesnions of this object is one column and 100 rows. 
head(data)
dim(data)
#convert object into vector
svl <- setNames(data$svl, rownames(data))

#let us estimate how large the ancestors were
Ancestors <- fastAnc(tree, svl, vars=TRUE, CI=TRUE)

#question 3. The estimated values are stored in the ace element. The CI95 element is the 95 percent confidence interval. 
 ?fastAnc

#question 4. One estimation is that the uncertanity around ancestral states can be large or the uncertainity around ancestrall states can be small.   

#plot tree
par(mar=c(0.1,0.1,0.1,0.1))
plot(tree, type="fan", lwd=2, show.tip.label=F)

#put points instead of names at the tips
tiplabels(pch=16, cex=0.25*svl[tree$tip.label])

#add Ancestral states to the tree
nodelabels(pch=16, cex=0.25*Ancestors$ace)

#visualize data in different way
obj <- contMap(tree, svl, plot=F)
plot(obj, type="fan", legend=0.7*max(nodeHeights(tree)), sig=2, fsize=c(0.7, 0.9))

#add fossils in the data
fossilData <- data.frame(svl=log(c(25.4, 23.2, 17.7, 19.7, 24, 31)), tip1=c("aliniger", "aliniger", "occultus", "christophei", "cristatellus", "occultus"), tip2=c("chlorocyanus", "coelestinus", "monticola", "cybotes", "angusticeps", "angusticeps"))

#for each fossil, find what node corresponds to the Most Recent Common Ancestor (MRCA) of the pair of tips in the dataframe. 
#question 5. 
fossilNodes <- c()
nodeN <- c()

Node <- fastMRCA(tree, for (variable in vector) {
  
fossilData[i, "tip1"], fossilData[i in, "tip2"])
}
fossilNodes[i] <- fossilData[i in "svl")]
nodeN[i] <- Node
