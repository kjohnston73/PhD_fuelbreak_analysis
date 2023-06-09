# import libraries
library(DataExplorer) 
library(nlme) 
library(lme4) 
library(MuMIn)
library(dplyr)
library(ggplot2)
library(tidyr)


## read in csv files
cover_gap <- read.csv(file = "data/glmm_cover_gap_data.csv")

## convert integer explanatory variables to factors
cover_gap$imazapic_binary = factor(cover_gap$imazapic_binary)
cover_gap$year = factor(cover_gap$year)
cover_gap$aspect_card = factor(cover_gap$aspect_card)
cover_gap$graze = factor(cover_gap$graze)
cover_gap$sand <- cover_gap$sand*100
cover_gap$clay <- cover_gap$clay*100

## calculate percent difference
cover_gap <- cover_gap %>% 
  separate(plot,  
           into = c("plot_num", "plot_let"),
           sep = 3)
iag <-data.frame()
iag <- cover_gap[, c("plot_num", "plot_let", "LineKey", "IAG")]
iag_wide <- pivot_wider(iag, names_from = "plot_let", values_from = "IAG")
iag_wide$iag_diff <- iag_wide$G - iag_wide$A
iag_wide <- iag_wide %>% unite("plot_line", c("plot_num","LineKey"), sep = "_", remove = FALSE)

## subset data to not grazed and not grazed adjacent
cover_gap_sub <- subset(cover_gap, graze =='T' | graze == 'A'| graze == 'N')
cover_gap_sub <- cover_gap_sub %>% unite("plot_line", c("plot_num","LineKey"), sep = "_", remove = FALSE)
cover_gap_sub <- cover_gap_sub[ -c(2:18) ]
#attach fixed variables to ht_wide
data <- data.frame()
data <-merge(iag_wide, cover_gap_sub, by = "plot_line")

# GLMM process
# Step 1 - linear regression
M1 <- lm(IAG ~ aspect_card + graze + sand + clay + elev_scaled + year + precip + pdsi_prev_year + imazapic_binary, 
         data = data)
E1 <- rstandard(M1)
plot(E1, ylab = "standardized residuals")
abline(0,0)
plot_histogram(E1)
summary(M1)
AIC(M1) # 4068.025

# Step 2 - fit the gls
M2 <- gls(IAG ~ aspect_card + graze + sand + clay + elev_scaled + year, 
          method = "ML", data = data)
summary(M2)
AIC(M2) # 4068.025

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
M3 <- lmer(IAG ~ aspect_card + graze + sand + clay + elev_scaled + year + (1 | plot), REML = TRUE, data = data)
## compare gls to random-intercept lme
anova(M2,M3) # M3 AIC 1191.468 - lower than M2
summary(M3)
plot(M3)

## no variable makes sense to use as a random slope, skip

## Step 6 - check the residuals, add variance structure if needed
plot(M3, col = (data$imazapic_binary))#, cex=(data$graze)
## smaller variance for smaller fitted values, no clear correlation to any fixed variable
m3_res<-resid(M3, type = "normalized")
plot(m3_res)
abline(0,0)

# # add variance structures
# M5 <- lmer(herbload ~ aspect_card + graze + sand + clay + elev_scaled +
#             imazapic_binary + (1 | plot), REML = TRUE, data = data,
#           weights = varFixed(~elev_scaled))
# M6<- lmer(herbload ~ aspect_card + graze + sand + clay + elev_scaled +
#            imazapic_binary + (1 | plot), REML = TRUE, data = data,
#          weights = varIdent(~elev_scaled))
# M7<- lmer(herbload ~ aspect_card + graze + sand + clay + elev_scaled +
#            imazapic_binary + (1 | plot), REML = TRUE, data = data,
#          weights = varPower(form = ~elev_scaled))
# M8<- lmer(herbload ~ aspect_card + graze + sand + clay + elev_scaled +
#            imazapic_binary + (1 | plot), REML = TRUE, data = data,
#          weights = varExp(form = ~elev_scaled))
# M9<- lmer(herbload ~ aspect_card + graze + sand + clay + elev_scaled +
#            imazapic_binary + (1 | plot), REML = TRUE, data = data,
#          weights = varConstPower(form = ~elev_scaled))
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

# ## try adding spatial autocorrelation variograms
# sp1 <- corGaus(form = ~ UTM_EASTING + UTM_NORTHING, nugget = TRUE, fixed = FALSE)
# sp2 <- corExp(form = ~ UTM_EASTING + UTM_NORTHING, nugget = TRUE, fixed = FALSE)
# sp3 <- corSpher(form = ~ UTM_EASTING + UTM_NORTHING, nugget = TRUE, fixed = FALSE)
# sp4 <- corLin(form = ~ UTM_EASTING + UTM_NORTHING, nugget = TRUE, fixed = FALSE)
# sp5 <- corRatio(form = ~ UTM_EASTING + UTM_NORTHING, nugget = TRUE, fixed = FALSE)
# 
# m3a <- update(M3, correlation = sp1)
# m3b <- update(M3, correlation = sp2)
# m3c <- update(M3, correlation = sp3)
# m3d <- update(M3, correlation = sp4)
# m3e <- update(M3, correlation = sp5)
# 
# AIC(M3,m3a,m3b,m3c,m3d,m3e) # original M3 model has best AIC
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
M3.full <- lmer(iag_diff ~ aspect_card + graze + sand + clay + ELEVATION + year + imazapic_binary + (1 | plot_num), REML = TRUE, data = data)
drop1(M3.full)
#drop sand 
M3.full <- lmer(iag_diff ~ aspect_card + graze + clay + ELEVATION + year + imazapic_binary + (1 | plot_num), REML = TRUE, data = data)
drop1(M3.full)
#drop clay
M3.full <- lmer(iag_diff ~ aspect_card + graze + ELEVATION + year + imazapic_binary + (1 | plot_num), REML = TRUE, data = data)
drop1(M3.full)
#drop aspect?
M3.full <- lmer(iag_diff ~ graze + ELEVATION + year + imazapic_binary + (1 | plot_num), REML = TRUE, data = data)
drop1(M3.full)


#LFB
iag_fb <- lmer(iag_diff ~ aspect_card + graze + ELEVATION + year + imazapic_binary + (1 | plot_num), REML = TRUE, data = data)
summary(iag_fb)
plot(resid(iag_fb, type = "deviance"))
abline(0,0)
r.squaredGLMM(iag_fb)
