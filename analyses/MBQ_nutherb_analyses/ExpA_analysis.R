## MBQ nutrients and herbivory analysis
## Experiment A
## GREX Moorea Winer 2014

# name data table
ExpA <- ExpA_grugosa_masses
ExpA

# remove two rows of missing data (lost OSW treatments)
ExpA <- ExpA[-c(39,40), ]
ExpA

# box plots
plot(chg_mass ~ trt, data = ExpA)

# Fligner-Killeen test of homogeneity of variances
fligner.test(chg_mass ~ interaction(cage, sed_nut, watercol_nut), data = ExpA)


################# bar plotting -- doesn't work yet -- #######################
# find means by treatment
mean_chg <- aggregate(ExpA[ ,7], list(Treatment=ExpA$trt), mean)

# bar plot of means of chg in mass (not percent change)
# specify x-axis order
x1 <- factor(ExpA$trt, levels = c())

#plot
barplot(mean_chg$x, ylab = "Mean change in G. rugosa mass (g over 8 days)", xlab = "Treatment")