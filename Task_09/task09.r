#set working directory
setwd("C:\\Users\\Sherm\\Desktop\\Evolution\\Tasks\\Task_09")

#functions from phytools library
library(phytools)

#read phylogeny into R
text.string <- "(((((((cow, pig),whale),(bat,(lemur,human))),(robin,iguana)),coelacanth),(gold_fish,  trout)), shark);"
tree<-read.tree(text=text.string)
plot(tree, type="fan")

#question 1. there are 12 tips and branch lengths are present. there is a total of 22 branch lengths.  
tiplabels(frame="circle", bg='lightblue', cex=1)
tree$tip.label
edgelabels(tree$edge.length, bg="black", col="white", font=2)

#read data
data <- read.csv("https://jonsmitchell.com/data/svl.csv", stringsAsFactors=F, row.names=1)

#question2. 
dim(data)

#convert object into vector
svl <- setNames(data$svl, rownames(data))

#let us estimate how large the ancestors were
Ancestors <- fastAnc(tree, svl, vars=TRUE, CI=TRUE)

#question 3. 

#question 4. 

?fastAnc
#plot tree
par(mar=c(0.1,0.1,0.1,0.1))
plot(tree, type="fan", lwd=2, show.tip.label=F)

#put points at tips
tiplabels(pch=16, cex=0.25*svl[tree$tip.label])



