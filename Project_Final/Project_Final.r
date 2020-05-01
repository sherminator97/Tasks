setwd("C:\\Users\\Sherm\\Desktop\\Evolution\\Tasks\\Projectremodified\\Project_Final")
my_data <-read.csv("morph_datarevised.csv", header=TRUE)


#plot for species vs snount-vent length
pdf("Species vs. Snout-vent length.pdf", height=10, width=10)
par(las=1, mar=c(5, 5, 1, 1), mgp=c(2, 0.5, 0), tck=-0.01)
plot(my_data$Species, my_data$SVL, xlab="Species", ylab="Snount-vent length")
dev.off()


#plot for headwidth
pdf("Species vs. Head Width.pdf", height=10, width=10)
par(las=1, mar=c(5, 5, 1, 1), mgp=c(2, 0.5, 0), tck=-0.01)
plot(my_data$Species, my_data$HW, xlab="Species", ylab="Head Width")
dev.off()


#plot for species vs Fore-limb length
pdf("Species vs. Fore-limb length.pdf", height=10, width=10)
par(las=1, mar=c(5, 5, 1, 1), mgp=c(2, 0.5, 0), tck=-0.01)
plot(my_data$Species, my_data$FL, xlab="Species", ylab="Fore-limb length")
dev.off()



#plot for species vs Rear-Limb length
pdf("Species vs. Rear-limb length.pdf", height=10, width=10)
par(las=1, mar=c(5, 5, 1, 1), mgp=c(2, 0.5, 0), tck=-0.01)
plot(my_data$Species, my_data$RL, xlab="Species", ylab="Rear-limb length")
dev.off()
#For my analysis plan, I would compare the body sizes of species of Australian lizards that live in the same ecotype habitat and see if they are significantly different. 
#I would build plots for individual body parts vs species.
#For my final project, I would do MANOVA tests for each body part to determine if each body part is significantly or not significantly different. 
#I would also build a phylogeny to compare the species in the same genus. 


#plot for species vs ear to limb length
pdf("Species vs. Ear to limb length.pdf", height=10, width=10)
par(las=1, mar=c(5, 5, 1, 1), mgp=c(2, 0.5, 0), tck=-0.01)
plot(my_data$Species, my_data$NECK, xlab="Species", ylab="Ear to limb length")
dev.off()

#MANOVA
names(my_data)
bodyparts.manova <- manova(cbind(SVL, FL, RL, NECK, HW ) ~ Species, data = my_data)
summary(bodyparts.manova)

summary.aov(bodyparts.manova)



#plot for species vs forelimb length
pdf("Species vs. snout-vent length2.pdf", height=10, width=10)
par(las=1, mar=c(5, 5, 1, 1), mgp=c(2, 0.5, 0), tck=-0.01)
plot(my_data$Species, my_data$SVL, xlab="Species", ylab="snout-vent length")
dev.off()