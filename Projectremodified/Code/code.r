setwd("C:\\Users\\Sherm\\Desktop\\Evolution\\Tasks\\Projectremodified\\Analysis_Plan")
my_data <-read.csv("morph_datarevised.csv", header=TRUE)



#chisquare test for forelimb length
morph.data <- data.frame(my_data$Species, my_data$RL)
morph.data = table(my_data$Species, my_data$RL)
print(morph.data)
print(chisq.test(morph.data))

#plot for species vs forelimb length
par(las=1, mar=c(5, 5, 1, 1), mgp=c(2, 0.5, 0), tck=-0.01)
plot(my_data$Species, my_data$FL, xlab="Species", ylab="Forelimb Length")

#chi square test for snout-vent length
morph.data2 <- data.frame(my_data$Species, my_data$SVL)
morph.data2 = table(my_data$Species, my_data$SVL)
print(morph.data2)
print(chisq.test(morph.data2))

#plot for species vs snount-vent length
par(las=1, mar=c(5, 5, 1, 1), mgp=c(2, 0.5, 0), tck=-0.01)
plot(my_data$Species, my_data$SVL, xlab="Species", ylab="Snount-vent length")

#chisquare test for Rear-limb length
morph.data3 <- data.frame(my_data$Species, my_data$RL)
morph.data3 = table(my_data$Species, my_data$RL)
print(morph.data3)
print(chisq.test(morph.data3))

#plot for species vs Rear-limb length
par(las=1, mar=c(5, 5, 1, 1), mgp=c(2, 0.5, 0), tck=-0.01)
plot(my_data$Species, my_data$RL, xlab="Species", ylab="Rear-Limb length")

#chisquare test for hindlimb length
morph.data4 <- data.frame(my_data$Species, my_data$HL)
morph.data4 = table(my_data$Species, my_data$HL)
print(morph.data4)
print(chisq.test(morph.data4))

#plot for species vs Rear-limb length
par(las=1, mar=c(5, 5, 1, 1), mgp=c(2, 0.5, 0), tck=-0.01)
plot(my_data$Species, my_data$HL, xlab="Species", ylab="Hind-Limb length")

#chisquare test for head height
morph.data5 <- data.frame(my_data$Species, my_data$HH)
morph.data5 = table(my_data$Species, my_data$HH)
print(morph.data5)
print(chisq.test(morph.data5))

#plot for species vs head height
par(las=1, mar=c(5, 5, 1, 1), mgp=c(2, 0.5, 0), tck=-0.01)
plot(my_data$Species, my_data$HH, xlab="Species", ylab="Head Height")

#chi square test for head width
morph.data6 <- data.frame(my_data$Species, my_data$HW)
morph.data6 = table(my_data$Species, my_data$HW)
print(morph.data6)
print(chisq.test(morph.data6))

#plot for species vs. head width
par(las=1, mar=c(5, 5, 1, 1), mgp=c(2, 0.5, 0), tck=-0.01)
plot(my_data$Species, my_data$HW, xlab="Species", ylab="Head Width")

#chisquare test for snout length
morph.data7 <- data.frame(my_data$Species, my_data$SE)
morph.data7 = table(my_data$Species, my_data$SE)
print(morph.data7)
print(chisq.test(morph.data7))

#plot for species vs snout length
par(las=1, mar=c(5, 5, 1, 1), mgp=c(2, 0.5, 0), tck=-0.01)
plot(my_data$Species, my_data$SE, xlab="Species", ylab="Snout length")


#chisquare test for eye to ear length
morph.data8 <- data.frame(my_data$Species, my_data$CHEEK)
morph.data8 = table(my_data$Species, my_data$CHEEK)
print(morph.data8)
print(chisq.test(morph.data8))

#plot for species vs eye to ear length
par(las=1, mar=c(5, 5, 1, 1), mgp=c(2, 0.5, 0), tck=-0.01)
plot(my_data$Species, my_data$CHEEK, xlab="Species", ylab="Eye to ear length")

#chisquare test for ear to limb length
morph.data9 <- data.frame(my_data$Species, my_data$NECK)
morph.data9 = table(my_data$Species, my_data$NECK)
print(morph.data9)
print(chisq.test(morph.data9))

#plot for species vs eye to ear length
par(las=1, mar=c(5, 5, 1, 1), mgp=c(2, 0.5, 0), tck=-0.01)
plot(my_data$Species, my_data$CHEEK, xlab="Species", ylab="Ear to limb length")










