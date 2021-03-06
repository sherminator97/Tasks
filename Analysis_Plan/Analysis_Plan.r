setwd("C:\\Users\\Sherm\\Desktop\\Evolution\\Tasks\\Projectremodified\\Project_Draft")
my_data <-read.csv("morph_datarevised.csv", header=TRUE)

#SVL ANOVA
Ruber <- c(39.04,	36.07,	34.93,	37.35,	33.3,	35.14,	36.96,	33.96,	35.63,	36.27,	34.8,	40.5,	36.94,	26.38,	40.18,	37.73,	33.58,	34.13,	32.08,	29.84,	40.78,	37.4,	39.56,	30.49,	32.92,	36.57,	27.73,	39.48,	38.27,	34.77,	34.48
)
buchanani <- c(30.15,	38.08, 33.72, 37.08, 35.63,	38.34,	39.14,	37.17,	42.25,	39.53,	31.92,	31.5,	40.6,	38.44,	34.3,	43.07,	39.27,	41.07,	40.13,	38.07,	34.55,	33.03,	28.37,	36.49,	36.3,	29.7,	38.36,	32.95,	36.29,	40.28,	38.8,	34.78,	33.5,	36.1,	32.04,	36.82,	37.43,	38.97,	40.75,	39.63,	41.62,	46.6,	42.58,	41.73
)
metallicus <- c(43.61,	39.3,	45.24,	37.32,	42,	38.42,	40.09,	44.03,	31.45,	38.11,	41.72,	44.77,	39.38,	37.71,	47.24,	45.42,	40.2,	39.58,	40.26,	36.24,	39.62,	39.07,	39.27,	47.24,	38.28
)
A.cristatellus <- c(44,	45,	43, 47.5,	65,	60.5,	41.5,	66,	41.5,	58,	47,	60,	57.5,	68,	51,	72.5,	50,	43,	48,	41.5,	57,	68,	47,	49, 65.5,	63,	66,	49,	63.5,	47,	40.5,	50,	47.5,	45.5,	72,	65.5,	64,	50,	67,	66,	48.5,	42,	66,	45,	46,	45,	64,	45,	59.5,	67,	50,	49,	62.5,	61.5,	62,	57.5,	68.5,	49.5,	73,	50.5,	50,	70.5,	69,	70,	51,	50)

A.stratulus <- c(42,	49,	46,	43,	41,	46.5,	46,	48,	41,	45,	42.5,	45, 45,	46,	45,	35.5,	45.5,	48,	44,	40.5,	47,	36,	38.8,	42, 38.5,	38,	36,	36,	37,	48,	44.5
)

Combined_Groups <- data.frame(cbind(Ruber, buchanani, metallicus, A.cristatellus, A.stratulus))
summary(Combined_Groups)

Stacked_Groups <- stack(Combined_Groups)
Anova_Results <- aov(values ~ ind, data =Stacked_Groups)
summary(Anova_Results)

#plot for species vs snount-vent length
pdf("Species vs. Snout-vent length.pdf", height=10, width=10)
par(las=1, mar=c(5, 5, 1, 1), mgp=c(2, 0.5, 0), tck=-0.01)
plot(my_data$Species, my_data$SVL, xlab="Species", ylab="Snount-vent length")
dev.off()

#Head Width Anova
Ruber <- c(5.22,	4.43,	4.41,	4.85,	4.59,	4.64,	4.61,	4.47,	4.61,	4.55,	4.72,	5.65,	5.1,	3.43,	5.25,	4.56,	4.32,	4.4,	4.32,	3.85,	4.96,	4.39,	4.95,	4.41,	4.48,	4.89,	3.84,	4.99,	4.77,	4.19,	4.55)
buchanani <- c(3.93,	4.57,	4.11,	4.07,	4.83,	4.61,	5.37,	4.4,	4.82,	4.54,	4.16,	4.16,	5.2,	4.52,	4.31,	5.46,	4.83,	4.9,	4.47,	4.04,	3.88,	4.13,	3.95,	4.35,	4.2,	4.22,	4.53,	4.7,	4.44,	4.64,	5.12,	4.28,	4.44,	4.51,	4.04,	4.4,	4.93,	4.6,	4.88,	4.57,	5.21,	5.64,	5.97, 5.44
)
metallicus <- c(5.05,	5.27,	5.02,	5.1,	5.52,	5.03,	5.22,	5.63,	4.49,	4.68,	4.8,	5.06,	4.78,	4.77,	5.63,	6.64,	4.72,	5.12,	5.23,	4.9,	5.34,	4.94,	4.81,	5.4,	4.68
)

A.cristatellus <- c(7.8716,	7.566,	7.6171,	8.3795,	11.2322,	10.6326,	6.9267,	11.7531,	7.2443,	10.4618,	7.7984,	10.4749,	9.9436,	12.1699,	7.9828,	12.5083,	8.3122,	7.2712,	7.8827,	7.1758,	10.2319,	12.0874,	7.3573,	8.0466,	11.538,	10.9294,	11.4319,	8.1777,	11.7943,	8.1732,	6.9401,	8.6948,	8.0734,	8.0664,	12.9225,	12.0179,	11.5136,	8.0903,	12.2804,	11.8983,	8.1232,	7.3605,	12.1676,	7.3915,	12.1521,	7.5551,	10.5193,	11.9913,	7.9146,	7.8827,	10.5781,	10.5681,	10.991,	10.061,	12.1044,	8.4602,	13.0484,	7.9777,	8.138,	12.9529,	12.4755,	12.3734,	7.7441,	8.3686)

A.stratulus <- c(6.2752,	7.4326,	7.2261,	6.1848,	5.8828,	7.0302,	6.9943,	7.838,	6.3086,	7.0249,	6.2131,	7.0831,	7.2135,	7.1657,	7.1967,	5.8004,	7.1192,	7.054,	6.5923,	6.2581,	7.3921,	5.9904,	5.8843,	6.8319,	5.8806,	6.2669,	6.1154,	5.5991,	5.6847,	7.2493,	6.8263)

Combined_Groups <- data.frame(cbind(Ruber, buchanani, metallicus, A.cristatellus, A.stratulus))
summary(Combined_Groups)
Stacked_Groups <- stack(Combined_Groups)
Anova_Results <- aov(values ~ ind, data =Stacked_Groups)
summary(Anova_Results)
#plot for headwidth
pdf("Species vs. Head Width.pdf", height=10, width=10)
par(las=1, mar=c(5, 5, 1, 1), mgp=c(2, 0.5, 0), tck=-0.01)
plot(my_data$Species, my_data$HW, xlab="Species", ylab="Head Width")
dev.off()

#FL ANOVA
Ruber <- c(13.7,	13.72,	14.33,	13.68,	13.01,	14.11,	13.12,	14,	13.84,	13.68,	13.44,	14.53,	12.8,	9.35,	15.04,	13.5,	13.15,	13.07,	11.87,	11.27,	15,	14.41,	13.41,	11.88,	12.52,	13.34,	10.98,	14.55,	14.76,	13.75,	14.61
)
buchananii <- c(11.22,	14.73,	12.31,	13.64,	14.56,	15.86,	15.34,	14.08,	16.46,	13.81,	11.9,	12.14,	15.75,	12.75,	12.76,	15.55,	15,	15.25,	13.73,	13.73,	13.45,	12.03,	10.7,	13.7,	14.72,	11.2,	13.96,	12.08,	12.1,	14.21,	14.06,	13.14,	13.31,	14.5,	13.41,	14.72,	15.04,	13.77,	15.36,	14.81,	14.55,	15.42,	15.55,	14.3
)
metallicus <- c(14.6,	14.58,	15.83,	12.37,	14.94,	14.09,	13.5,	14.08,	10.73,	13.6,	14.24,	14.61,	14.06,	12.94,	14.82,	15.77,	13.82,	14.06,	13.27,	12.54,	13.78,	12.46,	13.13,	14.28,	13.05
)
Combined_Groups <- data.frame(cbind(Ruber, buchananii, metallicus))
summary(Combined_Groups)
Stacked_Groups <- stack(Combined_Groups)
Anova_Results <- aov(values ~ ind, data =Stacked_Groups)
summary(Anova_Results)

#plot for species vs Fore-limb length
pdf("Species vs. Fore-limb length.pdf", height=10, width=10)
par(las=1, mar=c(5, 5, 1, 1), mgp=c(2, 0.5, 0), tck=-0.01)
plot(my_data$Species, my_data$FL, xlab="Species", ylab="Fore-limb length")
dev.off()

#RL ANOVA
Ruber <- c(17.76,	16.71,	17.12,	16.18,	17.02,	16.71,	16.55,	17.14,	17.06,	17.25,	16.33,	17.44,	16.64,	12.07,	17.8,	16.66,	15.27,	15.34,	14.17,	13.42,	18.82,	18.17,	18.08,	15.03,	15.37,	16.96,	13.1,	18.32,	17.76,	16.98,	16.6
)
buchananii <- c(14.27,	17.92,	14.73,	16.96,	17.85,	19.48,	19.98,	18.58,	19.29,	16.77,	15.91,	14.46,	21.12,	17.7,	15.34,	17.6,	18.27,	18.28,	17.02,	16.81,	16.08,	14.59,	12.31,	17.36,	17.4,	14.82,	17.97,	15.07,	15.95,	17.07,	17.12,	15.6,	16.7,	17.33,	16.74,	17.73,	18,	17.24,	18.14,	18.7,	18,	18.25,	19.48,	17.95
)
metallicus <- c(16.7,	18.02,	18.01,	15.05,	18.78,	16.84,	16.92,	17.73,	12.25,	15.88,	17,	18.45,	17.13,	16.33,	17.35,	18.62,	16.06,	17.87,	16.9,	15.25,	17.93,	14.03,	15.54,	17.53,	16.35
)
Combined_Groups <- data.frame(cbind(Ruber, buchananii, metallicus))
summary(Combined_Groups)
Stacked_Groups <- stack(Combined_Groups)
Anova_Results <- aov(values ~ ind, data =Stacked_Groups)
summary(Anova_Results)

#plot for species vs Rear-Limb length
pdf("Species vs. Rear-limb.pdf", height=10, width=10)
par(las=1, mar=c(5, 5, 1, 1), mgp=c(2, 0.5, 0), tck=-0.01)
plot(my_data$Species, my_data$RL, xlab="Species", ylab="Rear-limb length")
dev.off()
#For my analysis plan, I would compare the body sizes of species of Australian lizards that live in the same ecotype habitat and see if they are significantly different. 
#I would build plots for individual body parts vs species.
#For my final project, I would chisquare tests for each body part to determine if each body part is significantly or not significantly different. 
#I would also build a phylogeny to compare the species in the same genus. 

#NECK ANOVA
ruber <- c(9.98,	7.99,	8.61,	8.4,	7.93,	7.3,	8.22,	8.52,	7.29,	8.12,	8.13,	8.64,	7.52,	5.75,	8.44,	8.58,	6.7,	6.75,	6.55,	6.02,	9.04,	7.95,	8.84,	6.06,	6.58,	7.2,	5.59,	7.37,	8.5,	7.39,	8.33
)
buchananii <- c(7.7,	7.48,	7.49,	8.03,	7.61,	8.05,	7.52,	8.29,	9.05,	8.64,	7.31,	7.88,	8.63,	7.21,	7.27,	9.49,	8.97,	8.94,	8.33,	7.26,	7.49,	6.83,	6,	7.67,	7.15,	6.73,	8.17,	6.55,	8.03,	8.95,	6.91,	8,	9.15,	7.74,	6.4,	7.66,	8.87,	7.92,	8.66,	8.71,	8.01,	9.96,	9.73,	8.91
)
metallicus <- c(8.49,	8.66,	8.38,	7.73,	9.35,	7.9,	8.38,	9.57,	7.18,	9.55,	8.69,	8.83,	8.41,	8.56,	9.43,	10.1,	8.33,	8.91,	8.25,	8.08,	7.8,	7.58,	7.5,	9.06,	6.93
)
Combined_Groups <- data.frame(cbind(ruber, buchananii, metallicus))
summary(Combined_Groups)
Stacked_Groups <- stack(Combined_Groups)
Anova_Results <- aov(values ~ ind, data =Stacked_Groups)
summary(Anova_Results)

#plot for species vs ear to limb length
pdf("Species vs. Ear to limb length.pdf", height=10, width=10)
par(las=1, mar=c(5, 5, 1, 1), mgp=c(2, 0.5, 0), tck=-0.01)
plot(my_data$Species, my_data$NECK, xlab="Species", ylab="Ear to limb length")
dev.off()








