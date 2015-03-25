##### MBQ nutrients and herbivory analysis
##### Experiment A
##### GREX Moorea Winer 2014

## name data table
ExpA <- ExpA_grugosa_masses
ExpA

## remove two rows of missing data (lost OSW treatments)
ExpA <- ExpA[-c(39,40), ]
ExpA

## residuals
r1 <- lm(ExpA$chg_mass ~ ExpA$trt)
resid(r1)
qqnorm(resid(r1))
qqline(resid(r1))                       # qq plot of residuals appears leptokurtic
# histogram
hist(resid(r1))                         # histogram of residuals appears left skewed
# residual vs. chg_mass
plot(resid(r1) ~ ExpA$chg_mass)


## transform to equalize variances
ExpA$trans_chg_mass <- 1/(ExpA$chg_mass)

## box plots
plot(trans_chg_mass ~ trt, data = ExpA)

## Fligner-Killeen test of homogeneity of variances
fligner.test(trans_chg_mass ~ interaction(cage, sed_nut, watercol_nut), data = ExpA)


################# bar plotting -- doesn't work yet -- #######################
# find means by treatment
mean_chg <- aggregate(ExpA[ ,7], list(Treatment=ExpA$trt), mean)

# bar plot of means of chg in mass (not percent change)
# specify x-axis order
x1 <- factor(ExpA$trt, levels = c())

#plot
barplot(mean_chg$x, ylab = "Mean change in G. rugosa mass (g over 8 days)", xlab = "Treatment")