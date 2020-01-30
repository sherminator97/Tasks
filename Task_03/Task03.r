#Task03

#install package
install.packages("paleobioDB", dep = T)

#set directory and activate functions
setwd("C:\\Users\\Sherm\\Desktop\\Evolution\\Tasks\\Task_03")
library(paleobioDB)

#download data for taxon
Taxon <- "Dinosauria"

#controlling time window
MinMA <- 66
MaxMA <- 252
fossils <- pbdb_occurrences(base_name = Taxon, show = c("phylo", "coords", "ident"), min_ma=MinMA, max_ma=MaxMA)

#how many species are known for each time period? there are approximatelty 70 species known for each time period.  
Res <- 5
nspeciesOverTime <- pbdb_richness(fossils, rank = "genus", temporal_extent = c(MaxMA,MinMA), res=Res)

#alternative plot
par(mar=c(4, 5, 2, 1), las=1, tck=-0.01, mgp=c(2.5, 0.5, 0))
plot(seq(to=MaxMA, from=MinMA, length.out=nrow(nspeciesOverTime)), nspeciesOverTime[,2], xlim=c(MaxMA, MinMA), type="l", xlab="age(millions of years ago)", ylab="num. of species", main = Taxon)

#get the appearance data
newspeciesOverTime <- pbdb_orig_ext(fossils, res=5, rank= "species", temporal_extent=c(MinMA, MaxMA))

#set up the plot
par(mar=c(4, 5, 2, 1), las=1, tck=-0.01, mgp=c(2.5, 0.5, 0))

#plot the first appearances
plot(seq(to=MaxMA, from=MinMA, length.out=nrow(newspeciesOverTime)), newspeciesOverTime[,1], xlim=c(MaxMA, MinMA), type="l", xlab="age(millions of years ago)", ylab="num. of species", main= Taxon)

#add a line for the last appearances
lines(seq(to=MaxMA, from=MinMA, length.out=nrow(newspeciesOverTime)), newspeciesOverTime[,2], col="red")

#add a legend. It surprises me that dinosaurs appeared first 100 million years ago. 
legend("topleft", legend=c("first appear", "go extinct"), col=c("black", "red"), lty=1, bty="n")

#set color for map
OceanCol <- "light blue"
LandCol <- "black"

#set colors for fossil occurences
http://colorbrewer2.org/
Cols <- c("#fee5d9", "#fcae91", "#fb6a4a", "#de2d26", "#a50f15")

#make map
par(las=0)
pbdb_map_richness(fossils, col.ocean=OceanCol, col.int = LandCol, col.rich=Cols)

#Geological Timescale
https://www.geosociety.org/documents/gsa/timescale/timescl.pdf

#Triassic fossils
MinMA <- 201
MaxMA <- 252
triassic_fossils <- pbdb_occurrences(base_name = Taxon, show = c("phylo", "coords", "ident"), min_ma=MinMA, max_ma=MaxMA)

#Jurassic Fossils
MinMA <- 145
MaxMA <- 201
jurassic_fossils <- pbdb_occurrences(base_name = Taxon, show = c("phylo", "coords", "ident"), min_ma=MinMA, max_ma=MaxMA)

#Cretaceous fossils
MinMA <- 66
MaxMA <- 145
cretaceous_fossils <- pbdb_occurrences(base_name = Taxon, show = c("phylo", "coords", "ident"), min_ma=MinMA, max_ma=MaxMA)

#make series of maps

#Triassic
dev.new(height = 7.8, width = 13)
pbdb_map_richness(triassic_fossils, col.ocean=OceanCol, col.int= LandCol, col.rich=Cols)
mtext(side = 3, "Triassic (252-201Ma)", cex=3, line=-2)

#Jurassic
dev.new(height = 7.8, width = 13)
pbdb_map_richness(jurassic_fossils, col.ocean=OceanCol, col.int=LandCol, col.rich=Cols)
mtext(side = 3, "Jurassic (201 - 145Ma)", cex=3, line=-2)

#Cretaceous
dev.new(height = 7.8, width = 13)
pbdb_map_richness(cretaceous_fossils, col.ocean=OceanCol, col.int=LandCol, col.rich=Cols)
mtext(side = 3, "Cretaceous (145 - 66Ma)", cex=3, line=-2)

#mammals
Taxon2 <- "Mammalia"
MinMA <- 66
MaxMA <- 252
fossils2 <- pbdb_occurrences(base_name = Taxon2, show = c("phylo", "coords", "ident"), min_ma=MinMA, max_ma=MaxMA)
nspeciesOverTime2 <- pbdb_richness(fossils2, rank = "genus", temporal_extent = c(MaxMA, MinMA), res=Res)

#plotting both groups
par(mar=c(4, 5, 2, 1), las=1, tck=-0.01, mgp=c(2.5, 0.5, 0))
Col_dino <- Cols[length(Cols)]
Col_mammal <- Cols[1]
LineWidth <- 2
plot(seq(to=MaxMA, from=MinMA, length.out=nrow(nspeciesOverTime)), nspeciesOverTime[,2], xlim=c(MaxMA, MinMA), type="l", xlab="age(millions of years ago)", ylab="num. of species", col=Col_dino, lwd=LineWidth)
lines(seq(to=MaxMA, from=MinMA, length.out=nrow(nspeciesOverTime2)), nspeciesOverTime2[,2], col = Col_mammal, lwd=LineWidth)
legend("topleft", legend=c(Taxon, Taxon2), col=c(Col_dino, Col_mammal), bty="n", lwd=LineWidth)

#extension set directory
setwd("C:\\Users\\Sherm\\Desktop\\Evolution\\Tasks\\Task_03")
library(paleobioDB)

#Echinodermata
Taxon3 <- "Echinodermata"
MinMA <- 82
MaxMA <- 251
fossils3 <- pbdb_occurrences(base_name = Taxon3, show = c("phylo", "coords", "ident"), min_ma=MinMA, max_ma=MaxMA)
Res <- 4
nspeciesOverTime3 <-  pbdb_richness(fossils3, rank = "genus", temporal_extent = c(MaxMA, MinMA), res=Res)

#alternative plot for echinoderms
par(mar=c(4, 5, 2, 1), las=1, tck=-0.01, mgp=c(2.5, 0.5, 0))
plot(seq(to=MaxMA, from=MinMA, length.out=nrow(nspeciesOverTime3)), nspeciesOverTime3[,2], xlim=c(MaxMA, MinMA), type="l", xlab="age(millions of years ago)", ylab="num. of species", main = Taxon3)

#apperance data for echinodermata
newspeciesOverTime3 <- pbdb_orig_ext(fossils3, res=4, rank= "species", temporal_extent=c(MinMA, MaxMA))

#set up the plot
par(mar=c(4, 5, 2, 1), las=1, tck=-0.01, mgp=c(2.5, 0.5, 0))

#plot first appearances for echinodermata
plot(seq(to=MaxMA, from=MinMA, length.out=nrow(newspeciesOverTime3)), newspeciesOverTime3[,1], xlim=c(MaxMA, MinMA), type="l", xlab="age(millions of years ago)", ylab="num. of species", main= Taxon3)

#add a line for the last appearances of echinodermata
lines(seq(to=MaxMA, from=MinMA, length.out=nrow(newspeciesOverTime3)), newspeciesOverTime3[,2], col="red")

#add a legend
legend("topleft", legend=c("first appear", "go extinct"), col=c("black", "red"), lty=1, bty="n")

#set color for map
OceanCol <- "light blue"
LandCol <- "black"

#set colors for fossil occurences
Cols <- c("#fee5d9", "#fcae91", "#fb6a4a", "#de2d26", "#a50f15")

#make map
par(las=0)
pbdb_map_richness(fossils3, col.ocean=OceanCol, col.int = LandCol, col.rich=Cols)

#Triassic fossils3
MinMA <- 200
MaxMA <- 251
triassic_fossils3 <- pbdb_occurrences(base_name = Taxon3, show = c("phylo", "coords", "ident"), min_ma=MinMA, max_ma=MaxMA)

#Jurassic Fossils3
MinMA <- 144
MaxMA <- 199
jurassic_fossils3 <- pbdb_occurrences(base_name = Taxon3, show = c("phylo", "coords", "ident"), min_ma=MinMA, max_ma=MaxMA)

#Cretaceous fossils3
MinMA <- 65
MaxMA <- 143
cretaceous_fossils3 <- pbdb_occurrences(base_name = Taxon3, show = c("phylo", "coords", "ident"), min_ma=MinMA, max_ma=MaxMA)

#series of maps

#Triassic
dev.new(height = 7.8, width = 13)
pbdb_map_richness(triassic_fossils3, col.ocean=OceanCol, col.int= LandCol, col.rich=Cols)
mtext(side = 3, "Triassic (251 - 200Ma)", cex=3, line=-2)

#Jurassic
dev.new(height = 7.8, width = 13)
pbdb_map_richness(jurassic_fossils3, col.ocean=OceanCol, col.int=LandCol, col.rich=Cols)
mtext(side = 3, "Jurassic (199 - 144Ma)", cex=3, line=-2)

#Cretaceous
dev.new(height = 7.8, width = 13)
pbdb_map_richness(cretaceous_fossils3, col.ocean=OceanCol, col.int=LandCol, col.rich=Cols)
mtext(side = 3, "Cretaceous (143 - 65Ma)", cex=3, line=-2)





#Pseudosuchia
Taxon4 <- "Pseudosuchia"
MinMA <- 82
MaxMa <- 251
fossils4 <- pbdb_occurrences(base_name = Taxon4, show = c("phylo", "coords", "ident"), min_ma=MinMA, max_ma=MaxMA)
nspeciesOverTime4 <- pbdb_richness(fossils4, rank = "genus", temporal_extent = c(MaxMA, MinMA), res=Res)

#alternative plot for Pseudosuchia
par(mar=c(4, 5, 2, 1), las=1, tck=-0.01, mgp=c(2.5, 0.5, 0))
plot(seq(to=MaxMA, from=MinMA, length.out=nrow(nspeciesOverTime4)), nspeciesOverTime4[,2], xlim=c(MaxMA, MinMA), type="l", xlab="age(millions of years ago)", ylab="num. of species", main = Taxon4)

#get the apperance data
newspeciesOverTime4 <- pbdb_orig_ext(fossils4, res=4, rank= "species", temporal_extent=c(MinMA, MaxMA))

#set up the plot
par(mar=c(4, 5, 2, 1), las=1, tck=-0.01, mgp=c(2.5, 0.5, 0))

#plot first apperances for Pseudosuchia
plot(seq(to=MaxMA, from=MinMA, length.out=nrow(newspeciesOverTime4)), newspeciesOverTime4[,1], xlim=c(MaxMA, MinMA), type="l", xlab="age(millions of years ago)", ylab="num. of species", main= Taxon4)

#add a line for the last appearances of Pseudosuchia
lines(seq(to=MaxMA, from=MinMA, length.out=nrow(newspeciesOverTime4)), newspeciesOverTime4[,2], col="red")

#add a legend
legend("topleft", legend=c("first appear", "go extinct"), col=c("black", "red"), lty=1, bty="n")

#set color for map
OceanCol <- "light blue"
LandCol <- "black"

#set colors for fossil occurences
Cols <- c("#fee5d9", "#fcae91", "#fb6a4a", "#de2d26", "#a50f15")

#make map
par(las=0)
pbdb_map_richness(fossils4, col.ocean=OceanCol, col.int = LandCol, col.rich=Cols)

#Triassic fossils4
MinMA <- 200
MaxMA <- 251
triassic_fossils4 <- pbdb_occurrences(base_name = Taxon4, show = c("phylo", "coords", "ident"), min_ma=MinMA, max_ma=MaxMA)

#Jurassic Fossils4
MinMA <- 144
MaxMA <- 199
jurassic_fossils4 <- pbdb_occurrences(base_name = Taxon4, show = c("phylo", "coords", "ident"), min_ma=MinMA, max_ma=MaxMA)

#Cretaceous fossils4
MinMA <- 65
MaxMA <- 143
cretaceous_fossils4 <- pbdb_occurrences(base_name = Taxon4, show = c("phylo", "coords", "ident"), min_ma=MinMA, max_ma=MaxMA)

#series of maps

#Triassic4
dev.new(height = 7.8, width = 13)
pbdb_map_richness(triassic_fossils4, col.ocean=OceanCol, col.int= LandCol, col.rich=Cols)
mtext(side = 3, "Triassic (251 - 200Ma)", cex=3, line=-2)

#Jurassic4
dev.new(height = 7.8, width = 13)
pbdb_map_richness(jurassic_fossils4, col.ocean=OceanCol, col.int=LandCol, col.rich=Cols)
mtext(side = 3, "Jurassic (199 - 144Ma)", cex=3, line=-2)

#Cretaceous4
dev.new(height = 7.8, width = 13)
pbdb_map_richness(cretaceous_fossils4, col.ocean=OceanCol, col.int=LandCol, col.rich=Cols)
mtext(side = 3, "Cretaceous (143 - 65Ma)", cex=3, line=-2)


#plotting Echinoderms and Pseudosuchia
par(mar=c(4, 5, 2, 1), las=1, tck=-0.01, mgp=c(2.5, 0.5, 0))
Col_echino <- Cols[length(Cols)]
Col_pseudo <- Cols[1]
LineWidth <- 2
plot(seq(to=MaxMA, from=MinMA, length.out=nrow(nspeciesOverTime3)), nspeciesOverTime3[,2], xlim=c(MaxMA, MinMA), type="l", xlab="age(millions of years ago)", ylab="num. of species", col=Col_echino, lwd=LineWidth)
lines(seq(to=MaxMA, from=MinMA, length.out=nrow(nspeciesOverTime4)), nspeciesOverTime4[,2], col = Col_pseudo, lwd=LineWidth)
legend("topleft", legend=c(Taxon3, Taxon4), col=c(Col_echino, Col_pseudo), bty="n", lwd=LineWidth)

#my hyopothesis is that at around 80 millions of years ago, the number of the echinodermata species rose sharply than the pseudosuchia species due to a high reproduction rate.    
