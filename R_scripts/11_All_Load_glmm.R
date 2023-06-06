## import libraries
library(DataExplorer) #
library(nlme) #
library(lme4) #
# library(emmeans)
# library(ggplot2) 
# library(tidyr)
# library(janitor)
# library(dplyr)

## read in csv files
# cover_gap <- read.csv(file = "data/glmm_cover_gap_data.csv")
ht_load <- read.csv(file = "data/glmm_ht_load_data.csv")

## convert integer explanatory variables to factors
# cover_gap$imazapic_binary = factor(cover_gap$imazapic_binary)
# cover_gap$year = factor(cover_gap$year)
# cover_gap$aspect_card = factor(cover_gap$aspect_card)
# cover_gap$graze = factor(cover_gap$graze)

ht_load$imazapic_binary = factor(ht_load$imazapic_binary)
ht_load$year = factor(ht_load$year)
ht_load$aspect_card = factor(ht_load$aspect_card)
ht_load$graze = factor(ht_load$graze)

## subset data to not grazed and not grazed adjacent
#data <- subset(ht_load, graze =='N' | graze == 'AN')

# GLMM process
# Step 1 - linear regression
M1 <- lm(load_3root ~ aspect_deg + aspect_card + graze + sand + clay + ELEVATION +
           imazapic_binary + imazapic_months + year + age + precip + pdsi_prev_year + 
           pdsi_samp_year, 
         data = ht_load)
E1 <- rstandard(M1)
plot(E1, ylab = "standardized residuals")
abline(0,0)
plot_histogram(E1)
summary(M1)
AIC(M1) # 1728.126

# Step 2 - fit the gls
M2 <- gls(load_3root ~ aspect_deg + aspect_card + graze + sand + clay + ELEVATION +
            imazapic_binary + precip + pdsi_prev_year, method = "ML", data = ht_load)
summary(M2)
AIC(M2) # 3516.013

m2_res<-resid(M2, type = "normalized") # improved
plot(m2_res)
abline(0,0)

## add random value to utms to do spatial autocorrelation
add <- data.frame(matrix(ncol = 1, nrow =240))
add$rand <- (runif(240))
for (i in 1:nrow(data)){
  data$UTM_EASTING[i] <- data$UTM_EASTING[i] + add$rand[i]
}

## Step 3/4/5 - add random structures, compare models to find best fit
M3 <- lme(load_3root ~ aspect_deg + aspect_card + graze + sand + clay + ELEVATION +
            imazapic_binary + precip + pdsi_prev_year, random = ~1 | plot, method = "ML", data = ht_load)
## compare gls to random-intercept lme
anova(M2,M3) # M3 AIC 3455.248 - lower than M2
summary(M3)
plot(M3)

## no variable makes sense to use as a random slope, skip

## Step 6 - check the residuals, add variance structure if needed
plot(M3, col = 1, cex=((data$imazapic_binary)))
## smaller variance for smaller fitted values, no clear correlation to any fixed variable
m3_res<-resid(M3, type = "normalized")
plot(m3_res)
abline(0,0)

# # add variance structures
# M5 <- lme(herbload ~ aspect_deg + aspect_card + graze + sand + clay + ELEVATION +
#             imazapic_binary, random = ~1 | plot, method = "REML", data = data,
#           weights = varFixed(~ELEVATION))
# M6<- lme(herbload ~ aspect_deg + aspect_card + graze + sand + clay + ELEVATION +
#            imazapic_binary, random = ~1 | plot, method = "REML", data = data,
#          weights = varIdent(~ELEVATION))
# M7<- lme(herbload ~ aspect_deg + aspect_card + graze + sand + clay + ELEVATION +
#            imazapic_binary, random = ~1 | plot, method = "REML", data = data,
#          weights = varPower(form = ~ELEVATION))
# M8<- lme(herbload ~ aspect_deg + aspect_card + graze + sand + clay + ELEVATION +
#            imazapic_binary, random = ~1 | plot, method = "REML", data = data,
#          weights = varExp(form = ~ELEVATION))
# M9<- lme(herbload ~ aspect_deg + aspect_card + graze + sand + clay + ELEVATION +
#            imazapic_binary, random = ~1 | plot, method = "REML", data = data,
#          weights = varConstPower(form = ~ELEVATION))
# 
# anova(M3,M5) # no improvement
# anova(M3,M6) # no improvement
# anova(M3,M7) # big improvement ELEVATION, 3892.611
# anova(M3,M8) # small improvement aspect_deg, 3899.523
# anova(M3,M9) # no improvement
#
# m3_res<-resid(M3, type = "normalized")
# plot(m3_res)
# abline(0,0)
# plot(M3)
# plot(M5)
# plot(M10)

# variance structures did not improve AIC or residual plots

## try adding spatial autocorrelation variograms
sp1 <- corGaus(form = ~ UTM_EASTING + UTM_NORTHING, nugget = TRUE, fixed = FALSE)
sp2 <- corExp(form = ~ UTM_EASTING + UTM_NORTHING, nugget = TRUE, fixed = FALSE)
sp3 <- corSpher(form = ~ UTM_EASTING + UTM_NORTHING, nugget = TRUE, fixed = FALSE)
sp4 <- corLin(form = ~ UTM_EASTING + UTM_NORTHING, nugget = TRUE, fixed = FALSE)
sp5 <- corRatio(form = ~ UTM_EASTING + UTM_NORTHING, nugget = TRUE, fixed = FALSE)

m3a <- update(M3, correlation = sp1)
m3b <- update(M3, correlation = sp2)
m3c <- update(M3, correlation = sp3)
m3d <- update(M3, correlation = sp4)
m3e <- update(M3, correlation = sp5)

AIC(M3,m3a,m3b,m3c,m3d,m3e) # original M3 model has best AIC
# par(mfrow=c(1,1))
# plot(resid(M3, type = "normalized"))   # resid plots
# plot(resid(m3a, type = "normalized"))
# plot(resid(m3b, type = "normalized"))
# plot(resid(m3c, type = "normalized"))
# plot(resid(m3d, type = "normalized"))
# plot(resid(m3e, type = "normalized"))

# none of the spatial autocorrelation structures improve the AIC or the residuals

#step 7/8
# summary(M3) # AIC 1180.485
form <- formula(load_3root ~ aspect_deg + aspect_card + graze + sand + clay + ELEVATION +
                  imazapic_binary + precip + pdsi_prev_year)
M3.full <- lme(form, random = ~1 | plot, method = "ML", data = ht_load)
M3a <- update(M3.full, .~. -aspect_deg)
M3b <- update(M3.full, .~. -aspect_card)
M3c <- update(M3.full, .~. -graze)
M3d <- update(M3.full, .~. -sand)
M3e <- update(M3.full, .~. -clay)
M3f <- update(M3.full, .~. -imazapic_binary)
M3g <- update(M3.full, .~. -ELEVATION)
M3h <- update(M3.full, .~. -precip)
M3i <- update(M3.full, .~. -pdsi_prev_year)

anova(M3.full,M3a) # p 0.2237
anova(M3.full,M3b) # p 0.0272
anova(M3.full,M3c) # p 0.00005
anova(M3.full,M3d) # p 0.3204
anova(M3.full,M3e) # p 0.2215
anova(M3.full,M3f) # p 0.2896
anova(M3.full,M3g) # p 0.4340 ****
anova(M3.full,M3h) # p 0.0231 
anova(M3.full,M3i) # p 8e-04 

#drop elevation and go again
form2 <- formula(load_3root ~ aspect_deg + aspect_card + graze + sand + clay + 
                   imazapic_binary + precip + pdsi_prev_year)
M3.full2 <- lme(form2, random = ~1 | plot, method = "ML", data = ht_load)
M3a2 <- update(M3.full2, .~. -aspect_deg)
M3b2 <- update(M3.full2, .~. -aspect_card)
M3c2 <- update(M3.full2, .~. -graze)
M3d2 <- update(M3.full2, .~. -sand)
M3e2 <- update(M3.full2, .~. -clay)
M3f2 <- update(M3.full2, .~. -imazapic_binary)
M3g2 <- update(M3.full2, .~. -precip)
M3h2 <- update(M3.full2, .~. -pdsi_prev_year)

anova(M3.full2,M3a2) # p 0.2172
anova(M3.full2,M3b2) # p 0.0048
anova(M3.full2,M3c2) # p 1e-04
anova(M3.full2,M3d2) # p 0.5375 ****
anova(M3.full2,M3e2) # p 0.1630 
anova(M3.full2,M3f2) # p 0.2873
anova(M3.full2,M3g2) # p 0.0209
anova(M3.full2,M3h2) # p 7e-04

#drop sand and go again
form3 <- formula(load_3root ~ aspect_deg + aspect_card + graze + clay + 
                   imazapic_binary + precip + pdsi_prev_year)
M3.full3 <- lme(form3, random = ~1 | plot, method = "ML", data = ht_load)
M3a3 <- update(M3.full3, .~. -aspect_deg)
M3b3 <- update(M3.full3, .~. -aspect_card)
M3c3 <- update(M3.full3, .~. -graze)
M3d3 <- update(M3.full3, .~. -imazapic_binary)
M3f3 <- update(M3.full3, .~. -clay)
M3g3 <- update(M3.full3, .~. -precip)
M3h3 <- update(M3.full3, .~. -pdsi_prev_year)

anova(M3.full3,M3a3) # p 0.2422 
anova(M3.full3,M3b3) # p 0.0053
anova(M3.full3,M3c3) # p 1e-04
anova(M3.full3,M3d3) # p 0.2944 ****
anova(M3.full3,M3f3) # p 0.0173
anova(M3.full3,M3g3) # p 0.0205
anova(M3.full3,M3h3) # p 7e-04

#drop imazapic_binary and go again
form4 <- formula(load_3root ~ aspect_deg + aspect_card + graze + clay + precip + pdsi_prev_year)
M3.full4 <- lme(form4, random = ~1 | plot, method = "ML", data = ht_load)
M3b4 <- update(M3.full4, .~. -aspect_card)
M3c4 <- update(M3.full4, .~. -graze)
M3d4 <- update(M3.full4, .~. -clay)
M3f4 <- update(M3.full4, .~. -aspect_deg)
M3g4 <- update(M3.full4, .~. -precip)
M3h4 <- update(M3.full4, .~. -pdsi_prev_year)

anova(M3.full4,M3b4) # p 0.0069
anova(M3.full4,M3c4) # p 2e-04
anova(M3.full4,M3d4) # p 0.0164
anova(M3.full4,M3f4) # p 0.2613 ****
anova(M3.full4,M3g4) # p 0.0367 
anova(M3.full4,M3h4) # p 0.0012 

#drop aspect_deg and go again
form5 <- formula(load_3root ~ aspect_card + graze + clay + precip + pdsi_prev_year)
M3.full5 <- lme(form5, random = ~1 | plot, method = "ML", data = ht_load)
M3c5 <- update(M3.full5, .~. -graze)
M3d5 <- update(M3.full5, .~. -clay)
M3f5 <- update(M3.full5, .~. -aspect_card)
M3g5 <- update(M3.full5, .~. -precip)
M3h5 <- update(M3.full5, .~. -pdsi_prev_year)

anova(M3.full5,M3c5) # p 3e-04
anova(M3.full5,M3d5) # p 0.0239 ****
anova(M3.full5,M3f5) # p 0.0078
anova(M3.full5,M3g5) # p 0.046
anova(M3.full5,M3h5) # p 0.0017

#drop sand and go again
form6 <- formula(load_3root ~ graze + aspect_card)
M3.full6 <- lme(form6, random = ~1 | plot, method = "ML", data = data)
M3c6 <- update(M3.full6, .~. -graze)
M3d6 <- update(M3.full6, .~. -aspect_card)

anova(M3.full6,M3c6) # p 0.009 ****
anova(M3.full6,M3d6) # p 0.1072

#drop aspect_card and go again
form7 <- formula(load_3root ~ graze)
M3.full7 <- lme(form7, random = ~1 | plot, method = "ML", data = data)
M3d7 <- update(M3.full7, .~. -graze)

anova(M3.full7,M3d7) # p 0.0170


##
summary(M3.full7)
plot(resid(M3.full7, type = "normalized"))

qqnorm(M3.full7)
qqnorm(M3.full7, ~ranef (.),col =1)
intervals(M3.full7)