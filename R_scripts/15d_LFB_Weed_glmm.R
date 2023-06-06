## import libraries
library(DataExplorer) #
library(nlme) #
# library(lmerTest)
# library(lme4) #
# library(emmeans)
# library(ggplot2) 
library(tidyr)
# library(janitor)
# library(dplyr)

## read in csv files
cover_gap <- read.csv(file = "data/glmm_cover_gap_data.csv")
# ht_load <- read.csv(file = "data/glmm_ht_load_data.csv")

## convert integer explanatory variables to factors
cover_gap$imazapic_binary = factor(cover_gap$imazapic_binary)
cover_gap$year = factor(cover_gap$year)
cover_gap$aspect_card = factor(cover_gap$aspect_card)
cover_gap$graze = factor(cover_gap$graze)

## calculate percent difference
cover_gap <- cover_gap %>% 
  separate(plot,  
           into = c("plot_num", "plot_let"),
           sep = 3)
weed <-data.frame()
weed <- cover_gap[, c("plot_num", "plot_let", "LineKey", "WEED")]
weed_wide <- pivot_wider(weed, names_from = "plot_let", values_from = "WEED")
# iag_wide$A <- iag_wide$A + 0.01
# iag_wide$G <- iag_wide$G + 0.01
weed_wide$weed_diff <- weed_wide$G - weed_wide$A
weed_wide <- weed_wide %>% unite("plot_line", c("plot_num","LineKey"), sep = "_", remove = FALSE)

## subset data to not grazed and not grazed adjacent
cover_gap_sub <- subset(cover_gap, graze =='T' | graze == 'A'| graze == 'N')
cover_gap_sub <- cover_gap_sub %>% unite("plot_line", c("plot_num","LineKey"), sep = "_", remove = FALSE)
cover_gap_sub <- cover_gap_sub[ -c(2:19) ]
#attach fixed variables to ht_wide
data <- data.frame()
data <-merge(weed_wide, cover_gap_sub, by = "plot_line")

# GLMM process
# Step 1 - linear regression
M1 <- lm(WEED ~ aspect_card + graze + sand + clay + elev_scaled + year, 
         data = data)
E1 <- rstandard(M1)
plot(E1, ylab = "standardized residuals")
abline(0,0)
plot_histogram(E1)
summary(M1)
AIC(M1) # 669.2447

# Step 2 - fit the gls
M2 <- gls(WEED ~ aspect_card + graze + sand + clay + elev_scaled + year, 
          method = "ML", data = data)
summary(M2)
AIC(M2) # 669.2447

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
M3 <- lmer(WEED ~ aspect_card + graze + sand + clay + elev_scaled + year + (1 | plot), REML = TRUE, data = data)

## compare gls to random-intercept lme
anova(M2,M3) # M3 AIC 608.8783 - lower than M2
summary(M3)


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
#m3d <- update(M3, correlation = sp4)
m3e <- update(M3, correlation = sp5)

AIC(M3,m3a,m3b,m3c,m3e) # original M3 model has best AIC
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
M3.full <- lmer(weed_diff ~ aspect_card + graze + sand + clay + ELEVATION + year + imazapic_binary + (1 | plot_num), REML = TRUE, data = data)
drop1(M3.full)
#drop aspect
M3.full <- lmer(weed_diff ~ graze + sand + clay + ELEVATION + year + imazapic_binary + (1 | plot_num), REML = TRUE, data = data)
drop1(M3.full)
#drop year
M3.full <- lmer(weed_diff ~ graze + sand + clay + ELEVATION + imazapic_binary + (1 | plot_num), REML = TRUE, data = data)
drop1(M3.full)
#drop clay 
M3.full <- lmer(weed_diff ~ graze + sand + ELEVATION + imazapic_binary + (1 | plot_num), REML = TRUE, data = data)
drop1(M3.full)
#drop elevation
M3.full <- lmer(weed_diff ~ graze + sand + imazapic_binary + (1 | plot_num), REML = TRUE, data = data)
drop1(M3.full)
#drop graze
M3.full <- lmer(weed_diff ~ sand + imazapic_binary + (1 | plot_num), REML = TRUE, data = data)
drop1(M3.full)


#LFB
weed_fb <- lmer(weed_diff ~ sand + imazapic_binary + (1 | plot_num), REML = TRUE, data = data)
summary(weed_fb)
plot(resid(weed_fb, type = "deviance"))
abline(0,0)
r.squaredGLMM(weed_fb)
