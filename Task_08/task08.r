#load packages
library(phytools)
library(ape)

#input and plot tree
text.string <- "(((((((cow, pig), whale), (bat,(lemur, human))), (robin, iguana)), coelacanth), (gold_fish, trout)), shark);"
vert.tree <- read.tree(text=text.string)
plot(vert.tree, edge.width=2)

#number of nodes
nodelabels(frame="circle", bg='white', cex=1)

#question 1. a goldfish is more closely related to humans than a shark because they both are descended from the same most recent common ancestor.  . 

#look at object
vert.tree

#question 2. are there branch lengths in this tree? there are no branch lengths in this tree. 
str(vert.tree)

#use simpler tree to look into phylo object
tree <- read.tree(text="(((A,B), (C,D)), E);")
plotTree(tree, offset=1)
tiplabels(frame="circle", bg='lightblue', cex=1)
nodelabels(frame="circle", bg='white', cex=1)

#call tips
tree$tip.label

#look into phylo object's edge component
tree$edge

#phylogeny of Anolis lizards
AnolisTree <- force.ultrametric(read.tree("https://jonsmitchell.com/data/anolis.tre"))

#edge lengths of Anolis tree
par(las=1)
hist(AnolisTree$edge.length, col='black', border='white', main="", xlab="edge lengths for the Anolis tree", ylim=c(0, 50), xlim=c(0, 6))

tipEdges <- which(AnolisTree$edge[,2] <= Ntip(AnolisTree))
Lengths <- AnolisTree$edge.length
names(Lengths) <- AnolisTree$tip.label
names(Lengths)[which(Lengths == min(Lengths))]

plot(AnolisTree, cex=0.25)
Labs <- sapply(AnolisTree$edge.length, round, digits=2)
edgelabels(text=Labs, cex=0.25)

#Question 3. a tree with no tip labels
?plot.phylo
plot(AnolisTree, show.tip.label = FALSE, cex=0.25)
Labs <- sapply(AnolisTree$edge.length, round, digits=2)
edgelabels(text=Labs, cex=0.25)

#Question 4. a tree that is plotted as a circle instead of facing right or left
plot(AnolisTree, type = "radial", cex=0.25)
Labs <- sapply(AnolisTree$edge.length, round, digits=2)
edgelabels(text=Labs, cex=0.25)


#question 5. a tree with the tips colored red instead of black.
plot(AnolisTree, tip.color = "red", cex=0.25)
Labs <- sapply(AnolisTree$edge.length, round, digits=2)
edgelabels(text=Labs, cex=0.25)

#Question 6-8
tipEdges <- which(AnolisTree$edge[,2] <= Ntip(AnolisTree))
Lengths <- min(AnolisTree$edge.length)
names(Lengths) <- min(AnolisTree$tip.label)
names(Lengths)[which(Lengths == min(AnolisTree$edge.length))]
pr.species <- "ahli"
AnolisTree2 <-drop.tip(AnolisTree, pr.species)
plot(AnolisTree2, cex=0.25)
Labs <- sapply(AnolisTree2$edge.length, round, digits=2)
edgelabels(text=Labs, cex=0.25)

#look how fast species appeared in phylogeny
ltt(AnolisTree)
abline(0, 1, lwd=2, col='red', lty =2)

#Question 9
This graph tells me that the lineages arose sharply up till the present and the slope tells the stability throughout time. This slope tells us that these lizard's lineages were stable throughout time. 

#Question 10
?fit.bd()
fit.bd(AnolisTree, b=NULL, d=NULL, rho = 0.2)

