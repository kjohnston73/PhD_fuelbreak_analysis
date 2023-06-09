## import libraries
library(DataExplorer) 
library(nlme) 
library(lme4) 
library(MuMIn)
library(dplyr)
library(ggplot2)
library(tidyr)

## read in csv file
ht_load <- read.csv(file = "data/glmm_ht_load_data.csv")

## convert integer explanatory variables to factors
ht_load$imazapic_binary = factor(ht_load$imazapic_binary)
ht_load$year = factor(ht_load$year)
ht_load$aspect_card = factor(ht_load$aspect_card)
ht_load$graze = factor(ht_load$graze)
ht_load$sand <- ht_load$sand*100
ht_load$clay <- ht_load$clay*100

## calculate percent difference
ht_load <- ht_load %>% 
  separate(plot,  
           into = c("plot_num", "plot_let"),
           sep = 3)
ht <-data.frame()
ht <- ht_load[, c("plot_num", "plot_let", "LineKey", "max_height")]
ht_wide <- pivot_wider(ht, names_from = "plot_let", values_from = "max_height")
ht_wide$ht_diff <- ht_wide$G - ht_wide$A
ht_wide <- ht_wide %>% unite("plot_line", c("plot_num","LineKey"), sep = "_", remove = FALSE)
  
## subset data to not grazed and not grazed adjacent
ht_load_sub <- subset(ht_load, graze =='T' | graze == 'A'| graze == 'N')
ht_load_sub <- ht_load_sub %>% unite("plot_line", c("plot_num","LineKey"), sep = "_", remove = FALSE)
ht_load_sub <- ht_load_sub[ -c(2:21) ]
#attach fixed variables to ht_wide
data <- data.frame()
data <-merge(ht_wide, ht_load_sub, by = "plot_line")

# GLMM process
# Step 1 - linear regression
M1 <- lm(ht_diff ~ aspect_card + graze + sand + clay + ELEVATION + year, 
         data = data)
E1 <- rstandard(M1)
plot(E1, ylab = "standardized residuals")
abline(0,0)
plot_histogram(E1)
summary(M1)
AIC(M1) # 766.9835

# Step 2 - fit the gls
M2 <- gls(ht_diff ~ aspect_card + graze + sand + clay + ELEVATION + year, 
          method = "ML", data = data)
summary(M2)
AIC(M2) # 766.9835

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
M3 <- lmer(ht_diff ~ aspect_card + graze + sand + clay + elev_scaled + year + (1 | plot), REML = TRUE, data = data)
## compare gls to random-intercept lme
anova(M2,M3) # M3 AIC 745.0065 - lower than M2
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
M3.full <- lmer(ht_diff ~ aspect_card + graze + sand + clay + ELEVATION + year + imazapic_binary +(1 | plot_num), REML = TRUE, data = data)
drop1(M3.full)
#drop aspect
M3.full <- lmer(ht_diff ~ graze + sand + clay + ELEVATION + year + imazapic_binary + (1 | plot_num), REML = TRUE, data = data)
drop1(M3.full)
#drop imazapic
M3.full <- lmer(ht_diff ~ graze + sand + clay + ELEVATION + year + (1 | plot_num), REML = TRUE, data = data)
drop1(M3.full)


#LFB
ht_fb <- lmer(ht_diff ~ graze + sand + clay + ELEVATION + year + (1 | plot_num), REML = TRUE, data = data)
summary(ht_fb)
plot(resid(ht_fb, type = "deviance"))
abline(0,0)
r.squaredGLMM(ht_fb)
