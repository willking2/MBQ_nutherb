##### MBQ nutrients and herbivory analysis
##### Experiment A
##### GREX Moorea Winer 2014


########### summer 2015 analyses (took QSCI 482, 483)
ExpA <- read.csv(file.choose(), header = T)

# remove missing data
ExpA.na.omit <- na.omit(ExpA)

plot(ExpA) # not helpful
qqnorm(ExpA$chg_mass) # looks pretty good..

ExpA$cage <- as.factor(ExpA$cage)
ExpA$sed_nut <- as.factor(ExpA$sed_nut)
ExpA$watercol_nut <- as.factor(ExpA$watercol_nut)


#full linear, normal model 
# not weighted.. should it be?
modelA.1 <- aov(chg_mass ~ cage + sed_nut + watercol_nut + cage:sed_nut + cage:watercol_nut + sed_nut:watercol_nut + cage:sed_nut:watercol_nut, data = ExpA)
model.matrix(modelA.1)
summary(modelA.1)
anova(modelA.1) # signifcant three way interaction


modelA.2 <- aov(chg_mass ~ trt, data = ExpA)
model.matrix(modelA.1)
summary(modelA.2)
anova(modelA.2)

# studentized deleted residuals vs. model fits
plot(modelA.1$fit, rstudent(modelA.1), xlab = "model fit", ylab = "studentized deleted residuals", ylim = c(-3,3))
abline(h=0, lty=2)
abline(h=-2, lty=2)
abline(h=2, lty=2)
## looks like there might be 2 or 3 outliers? 
boxplot(rstudent(modelA.1))

## check for levereage points
# calculate differences between fitted values
DFFITS <- dffits(modelA.1)
DFFITS[DFFITS >1] # no values > 1, no leverage points

# cooks distances and corresponding p-values
cooks <- cooks.distance(modelA.1)
cooks[cooks >1]
cooks_pval <- pf(cooks, df1=8, df2=54, lower.tail=F) #df1 = number of parameters fitted, df2 = sample size - df1
cooks_pval[cooks_pval < 0.05] # no interesting/significant leverage points

# DFBETAS
DFBETAS <- dfbetas(modelA.1) 
DFBETAS[DFBETAS >1] # no leverage points

##### test for model appropriate-ness
## shapiro-wilks for departure from normality
shapiro.test(ExpA$chg_mass) # p = 0.7811, response var seems to be normally dist.

# interaction plots
#caged data
ExpA_caged <- subset(ExpA.na.omit, ExpA$cage == 1)
# caged plot
interaction.plot(ExpA_caged$sed_nut, ExpA_caged$watercol_nut, ExpA_caged$chg_mass, 
                 fun = mean,
                 type = c("l"), 
                 legend = FALSE,
                 trace.label = "water column nut",
                 fixed = TRUE,
                 xlab = "sediment nut",
                 ylab = "change in mass (g)",
                 ylim = c(0, 4),
                 main = "caged"
                 )
legend(x = "bottomright", legend = c("0", "1"), lty = c(2, 1), title = "water col nut", bty = "o", horiz = TRUE, y.intersp = 0.75)
# uncaged data
ExpA_uncaged <- subset(ExpA.na.omit, ExpA$cage == 0)
#uncaged plot
interaction.plot(ExpA_uncaged$sed_nut, ExpA_uncaged$watercol_nut, ExpA_uncaged$chg_mass,
                 fun = mean,
                 type = c("l"), 
                 legend = FALSE,
                 trace.label = "water column nut",
                 fixed = TRUE,
                 xlab = "sediment nut",
                 ylab = "change in mass (g)",
                 ylim = c(0, 4),
                 main = "uncaged"
                 )
legend(x = "topright", legend = c("0", "1"), lty = c(2, 1), title = "water col nut", bty = "o", horiz = TRUE, y.intersp = 0.75)

## multiple comparisons
TukeyHSD(modelA.1, "cage:sed_nut:watercol_nut", ordered = FALSE, conf.level = 0.95)
plot(TukeyHSD(modelA.1, "cage"))
plot(TukeyHSD(modelA.1, "sed_nut"))
plot(TukeyHSD(modelA.1, "watercol_nut"))
plot(TukeyHSD(modelA.1, "cage:watercol_nut"))
plot(TukeyHSD(modelA.1, "cage:sed_nut"))
plot(TukeyHSD(modelA.1, "sed_nut:watercol_nut"))
plot(TukeyHSD(modelA.1, "cage:sed_nut:watercol_nut"))

TukeyHSD(modelA.2, "trt", ordered = FALSE, conf.level = 0.95)
plot(TukeyHSD(modelA.2, "trt"))


#############################################################################
######### old analyses... (only took QSCI482) see below
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
