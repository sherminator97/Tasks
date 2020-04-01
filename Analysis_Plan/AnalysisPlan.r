setwd("C:\\Users\\Sherm\\Desktop\\Evolution\\Tasks\\Projectremodified\\Analysis_Plan")
my_data <-read.csv("morph_datarevised.csv", header=TRUE)

#For my analysis plan, I would compare the body sizes of species of Australian lizards that live in the same ecotype habitat and see if they are significantly different. 
#I would build plots for individual body parts vs species.
#For my final project, I would chisquare tests for each body part to determine if each body part is significantly or not significantly different. 
#I would also build a phylogeny to compare the species in the same genus. 

#chisquare test for forelimb length
morph.data <- data.frame(my_data$Species, my_data$FL)
morph.data = table(my_data$Species, my_data$FL)
print(morph.data)
print(chisq.test(morph.data))

#plot for species vs forelimb length
pdf("Species vs. Forelimb Length.pdf", height=8, width=8)
par(las=1, mar=c(5, 5, 1, 1), mgp=c(2, 0.5, 0), tck=-0.01)
plot(my_data$Species, my_data$FL, xlab="Species", ylab="Forelimb Length")
dev.off()

#chi square test for snout-vent length
morph.data2 <- data.frame(my_data$Species, my_data$SVL)
morph.data2 = table(my_data$Species, my_data$SVL)
print(morph.data2)
print(chisq.test(morph.data2))

#plot for species vs snount-vent length
pdf("Species vs. Snout-vent length.pdf", height=10, width=10)
par(las=1, mar=c(5, 5, 1, 1), mgp=c(2, 0.5, 0), tck=-0.01)
plot(my_data$Species, my_data$SVL, xlab="Species", ylab="Snount-vent length")
dev.off()

#chisquare test for Rear-limb length
morph.data3 <- data.frame(my_data$Species, my_data$RL)
morph.data3 = table(my_data$Species, my_data$RL)
print(morph.data3)
print(chisq.test(morph.data3))

#plot for species vs Rear-limb length
pdf("Species vs. Rear-limb length.pdf", height=8, width=8)
par(las=1, mar=c(5, 5, 1, 1), mgp=c(2, 0.5, 0), tck=-0.01)
plot(my_data$Species, my_data$RL, xlab="Species", ylab="Rear-Limb length")
dev.off()

#chisquare test for hindlimb length
morph.data4 <- data.frame(my_data$Species, my_data$HL)
morph.data4 = table(my_data$Species, my_data$HL)
print(morph.data4)
print(chisq.test(morph.data4))

#plot for species vs Hind-Limb length length
pdf("Species vs. Hind-Limb length.pdf", height=8, width=8)
par(las=1, mar=c(5, 5, 1, 1), mgp=c(2, 0.5, 0), tck=-0.01)
plot(my_data$Species, my_data$HL, xlab="Species", ylab="Hind-Limb length")
dev.off()

#chisquare test for head height
morph.data5 <- data.frame(my_data$Species, my_data$HH)
morph.data5 = table(my_data$Species, my_data$HH)
print(morph.data5)
print(chisq.test(morph.data5))

#plot for species vs head height
pdf("Species vs. Head height.pdf", height=8, width=8)
par(las=1, mar=c(5, 5, 1, 1), mgp=c(2, 0.5, 0), tck=-0.01)
plot(my_data$Species, my_data$HH, xlab="Species", ylab="Head Height")
dev.off()

#chi square test for head width
morph.data6 <- data.frame(my_data$Species, my_data$HW)
morph.data6 = table(my_data$Species, my_data$HW)
print(morph.data6)
print(chisq.test(morph.data6))

#plot for species vs. head width
pdf("Species vs head width.pdf", height=8, width=8)
par(las=1, mar=c(5, 5, 1, 1), mgp=c(2, 0.5, 0), tck=-0.01)
plot(my_data$Species, my_data$HW, xlab="Species", ylab="Head Width")
dev.off()

#chisquare test for snout length
morph.data7 <- data.frame(my_data$Species, my_data$SE)
morph.data7 = table(my_data$Species, my_data$SE)
print(morph.data7)
print(chisq.test(morph.data7))

#plot for species vs snout length
pdf("Species vs snout length.pdf", height=8, width=8)
par(las=1, mar=c(5, 5, 1, 1), mgp=c(2, 0.5, 0), tck=-0.01)
plot(my_data$Species, my_data$SE, xlab="Species", ylab="Snout length")
dev.off()

#chisquare test for eye to ear length
morph.data8 <- data.frame(my_data$Species, my_data$CHEEK)
morph.data8 = table(my_data$Species, my_data$CHEEK)
print(morph.data8)
print(chisq.test(morph.data8))

#plot for species vs eye to ear length
pdf("Species vs eye to ear length.pdf", height=8, width=8)
par(las=1, mar=c(5, 5, 1, 1), mgp=c(2, 0.5, 0), tck=-0.01)
plot(my_data$Species, my_data$CHEEK, xlab="Species", ylab="Eye to ear length")
dev.off()

#chisquare test for ear to limb length
morph.data9 <- data.frame(my_data$Species, my_data$NECK)
morph.data9 = table(my_data$Species, my_data$NECK)
print(morph.data9)
print(chisq.test(morph.data9))

#plot for species vs eye to limb length
pdf("Species vs eye to limb length.pdf", height=8, width=8)
par(las=1, mar=c(5, 5, 1, 1), mgp=c(2, 0.5, 0), tck=-0.01)
plot(my_data$Species, my_data$NECK, xlab="Species", ylab="Ear to limb length")










