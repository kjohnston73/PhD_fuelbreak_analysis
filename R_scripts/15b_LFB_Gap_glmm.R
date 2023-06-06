## import libraries
library(DataExplorer) #
# library(nlme) #
library(lme4) #
# library(emmeans)
# library(ggplot2) 
library(tidyr)
library(MuMIn)
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

## calculate difference
cover_gap <- cover_gap %>% 
  separate(plot,  
           into = c("plot_num", "plot_let"),
           sep = 3)
gap <-data.frame()
gap <- cover_gap[ -c(5:9)]
gap <- gap[-c(6:29)]
gap <- gap[-c(3)]
gap_mean_wide <- pivot_wider(gap, names_from = "plot_let", values_from = "gap_mean")
gap_mean_wide$gap_mean_diff <- gap_mean_wide$G - gap_mean_wide$A
gap_mean_wide <- gap_mean_wide %>% unite("plot_line", c("plot_num","LineKey"), sep = "_", remove = FALSE)

## subset data to not grazed and not grazed adjacent
cover_gap_sub <- subset(cover_gap, graze =='T' | graze == 'A'| graze == 'N')
cover_gap_sub <- cover_gap_sub %>% unite("plot_line", c("plot_num","LineKey"), sep = "_", remove = FALSE)
cover_gap_sub <- cover_gap_sub[ -c(2:19) ]
#attach fixed variables to ht_wide
data <- data.frame()
data <-merge(gap_mean_wide, cover_gap_sub, by = "plot_line")

# ht_load$imazapic_binary = factor(ht_load$imazapic_binary)
# ht_load$year = factor(ht_load$year)
# ht_load$aspect_card = factor(ht_load$aspect_card)
# ht_load$graze = factor(ht_load$graze)

## subset data to not grazed and not grazed adjacent
# data <- subset(ht_load, graze =='N' | graze == 'AN')
# data <- subset(cover_gap, graze =='T' | graze == 'A' | graze == 'N')

# GLMM process
# Step 1 - linear regression
M1 <- lm(gap_mean_diff ~ aspect_card + graze + sand + clay + ELEVATION + year, 
         data = data)
E1 <- rstandard(M1)
plot(E1, ylab = "standardized residuals")
abline(0,0)
plot_histogram(E1)
summary(M1)
AIC(M1) # 15.25375

# Step 2 - fit the gls
M2 <- gls(gap_mean_diff ~ aspect_card + graze + sand + clay + ELEVATION + year, 
          method = "ML", data = data)
summary(M2)
AIC(M2) # 15.25375

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
M3 <- lmer(gap_mean_diff ~ aspect_card + graze + sand + clay + ELEVATION + year + (1 | plot_num), REML = TRUE, data = data)
## compare gls to random-intercept lme
anova(M2,M3) # M3 AIC 9.562173 - lower than M2
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

AIC(M3,m3b,m3c,m3e) # original M3 model has best AIC
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
M3.full <- lmer(gap_mean_diff ~ aspect_card + graze + sand + clay + ELEVATION + year + imazapic_binary + (1 | plot_num), REML = TRUE, data = data)
drop1(M3.full)
#drop aspect
M3.full <- lmer(gap_mean_diff ~ graze + sand + clay + ELEVATION + year + imazapic_binary + (1 | plot_num), REML = TRUE, data = data)
drop1(M3.full)
#drop graze
M3.full <- lmer(gap_mean_diff ~ sand + clay + ELEVATION + year + imazapic_binary + (1 | plot_num), REML = TRUE, data = data)
drop1(M3.full)
#drop imazapic
M3.full <- lmer(gap_mean_diff ~ sand + clay + ELEVATION + year + (1 | plot_num), REML = TRUE, data = data)
drop1(M3.full)
#drop year
M3.full <- lmer(gap_mean_diff ~ sand + clay + ELEVATION + (1 | plot_num), REML = TRUE, data = data)
drop1(M3.full)
#drop clay
M3.full <- lmer(gap_mean_diff ~ sand + ELEVATION + (1 | plot_num), REML = TRUE, data = data)
drop1(M3.full)

#LFB
gap_mean_fb <- lmer(gap_mean_diff ~ sand + ELEVATION + (1 | plot_num), REML = TRUE, data = data)
summary(gap_mean_fb)
plot(resid(gap_mean_fb, type = "deviance"))
abline(0,0)
r.squaredGLMM(gap_mean_fb)





## calculate difference
gap <-data.frame()
gap <- cover_gap[ -c(5:11)]
gap <- gap[-c(6:27)]
gap <- gap[-c(3)]
gap_lt25_wide <- pivot_wider(gap, names_from = "plot_let", values_from = "gap_percent_lt25")
gap_lt25_wide$gap_lt25_diff <- gap_lt25_wide$G - gap_lt25_wide$A
gap_lt25_wide <- gap_lt25_wide %>% unite("plot_line", c("plot_num","LineKey"), sep = "_", remove = FALSE)
#attach fixed variables to ht_wide
data <- data.frame()
data <-merge(gap_lt25_wide, cover_gap_sub, by = "plot_line")

## gap_percent_lt25
M3.full <- lmer(gap_lt25_diff ~ aspect_card + graze + sand + clay + ELEVATION + year + imazapic_binary +(1 | plot_num), REML = TRUE, data = data)
drop1(M3.full)
## drop aspect
M3.full <- lmer(gap_lt25_diff ~ graze + sand + clay + ELEVATION + year + imazapic_binary +(1 | plot_num), REML = TRUE, data = data)
drop1(M3.full)
## drop imazapic
M3.full <- lmer(gap_lt25_diff ~ graze + sand + clay + ELEVATION + year + (1 | plot_num), REML = TRUE, data = data)
drop1(M3.full)
## drop elevation
M3.full <- lmer(gap_lt25_diff ~ graze + sand + clay + year + (1 | plot_num), REML = TRUE, data = data)
drop1(M3.full)
## drop year
M3.full <- lmer(gap_lt25_diff ~ graze + sand + clay + (1 | plot_num), REML = TRUE, data = data)
drop1(M3.full)
## drop clay
M3.full <- lmer(gap_lt25_diff ~ graze + sand + (1 | plot_num), REML = TRUE, data = data)
drop1(M3.full)

#LFB
gap_lt25_fb <- lmer(gap_lt25_diff ~ sand + graze + (1 | plot_num), REML = TRUE, data = data)
summary(gap_lt25_fb)
plot(resid(gap_lt25_fb, type = "deviance"))
abline(0,0)
r.squaredGLMM(gap_lt25_fb)


## calculate difference
gap <-data.frame()
gap <- cover_gap[ -c(5:12)]
gap <- gap[-c(6:26)]
gap <- gap[-c(3)]
gap_25_50_wide <- pivot_wider(gap, names_from = "plot_let", values_from = "gap_percent_25_50")
gap_25_50_wide$gap_25_50_diff <- gap_25_50_wide$G - gap_25_50_wide$A
gap_25_50_wide <- gap_25_50_wide %>% unite("plot_line", c("plot_num","LineKey"), sep = "_", remove = FALSE)
#attach fixed variables to ht_wide
data <- data.frame()
data <-merge(gap_25_50_wide, cover_gap_sub, by = "plot_line")
## gap_percent_25_50
M3.full <- lmer(gap_25_50_diff ~ aspect_card + graze + sand + clay + ELEVATION + year + imazapic_binary + (1 | plot_num), REML = TRUE, data = data)
drop1(M3.full)
## drop aspect
M3.full <- lmer(gap_25_50_diff ~ graze + sand + clay + ELEVATION + year + imazapic_binary + (1 | plot_num), REML = TRUE, data = data)
drop1(M3.full)
## drop elevation 
M3.full <- lmer(gap_25_50_diff ~ graze + sand + clay + year + imazapic_binary + (1 | plot_num), REML = TRUE, data = data)
drop1(M3.full)
## drop sand
M3.full <- lmer(gap_25_50_diff ~ graze + clay + year + imazapic_binary + (1 | plot_num), REML = TRUE, data = data)
drop1(M3.full)
## drop sand
M3.full <- lmer(gap_25_50_diff ~ graze + year + imazapic_binary + (1 | plot_num), REML = TRUE, data = data)
drop1(M3.full)
## drop year
M3.full <- lmer(gap_25_50_diff ~ graze + imazapic_binary + (1 | plot_num), REML = TRUE, data = data)
drop1(M3.full)
#LFB
gap_25_50_fb <- lmer(gap_25_50_diff ~ graze + imazapic_binary + (1 | plot_num), REML = TRUE, data = data)
summary(gap_25_50_fb)
plot(resid(gap_25_50_fb, type = "deviance"))
abline(0,0)
r.squaredGLMM(gap_25_50_fb)


## calculate difference
gap <-data.frame()
gap <- cover_gap[ -c(5:13)]
gap <- gap[-c(6:25)]
gap <- gap[-c(3)]
gap_50_100_wide <- pivot_wider(gap, names_from = "plot_let", values_from = "gap_percent_51_100")
gap_50_100_wide$gap_50_100_diff <- gap_50_100_wide$G - gap_50_100_wide$A
gap_50_100_wide <- gap_50_100_wide %>% unite("plot_line", c("plot_num","LineKey"), sep = "_", remove = FALSE)
#attach fixed variables to ht_wide
data <- data.frame()
data <-merge(gap_50_100_wide, cover_gap_sub, by = "plot_line")
## gap_percent_51_100
M3.full <- lmer(gap_50_100_diff ~ aspect_card + graze + sand + clay + ELEVATION + year + imazapic_binary +(1 | plot_num), REML = TRUE, data = data)
drop1(M3.full)
## drop aspect
M3.full <- lmer(gap_50_100_diff ~ graze + sand + clay + ELEVATION + year + imazapic_binary +(1 | plot_num), REML = TRUE, data = data)
drop1(M3.full)
## drop graze 
M3.full <- lmer(gap_50_100_diff ~ sand + clay + ELEVATION + year + imazapic_binary +(1 | plot_num), REML = TRUE, data = data)
drop1(M3.full)
## drop clay
M3.full <- lmer(gap_50_100_diff ~ sand + ELEVATION + year + imazapic_binary +(1 | plot_num), REML = TRUE, data = data)
drop1(M3.full)
## drop clay
M3.full <- lmer(gap_50_100_diff ~ sand + ELEVATION + year + imazapic_binary +(1 | plot_num), REML = TRUE, data = data)
drop1(M3.full)
## drop elevation
M3.full <- lmer(gap_50_100_diff ~ sand + year + imazapic_binary +(1 | plot_num), REML = TRUE, data = data)
drop1(M3.full)
## drop sand
M3.full <- lmer(gap_50_100_diff ~ year + imazapic_binary +(1 | plot_num), REML = TRUE, data = data)
drop1(M3.full)
#LFB
gap_50_100_fb <- lmer(gap_50_100_diff ~ year + imazapic_binary + (1 | plot_num), REML = TRUE, data = data)
summary(gap_50_100_fb)
plot(resid(gap_50_100_fb, type = "deviance"))
abline(0,0)
r.squaredGLMM(gap_50_100_fb)



## calculate difference
gap <-data.frame()
gap <- cover_gap[ -c(5:14)]
gap <- gap[-c(6:24)]
gap <- gap[-c(3)]
gap_100_200_wide <- pivot_wider(gap, names_from = "plot_let", values_from = "gap_percent_101_200")
gap_100_200_wide$gap_100_200_diff <- gap_100_200_wide$G - gap_100_200_wide$A
gap_100_200_wide <- gap_100_200_wide %>% unite("plot_line", c("plot_num","LineKey"), sep = "_", remove = FALSE)
#attach fixed variables to ht_wide
data <- data.frame()
data <-merge(gap_100_200_wide, cover_gap_sub, by = "plot_line")
## gap_percent_101_200
M3.full <- lmer(gap_100_200_diff ~ aspect_card + graze + sand + clay + ELEVATION + year + imazapic_binary +(1 | plot_num), REML = TRUE, data = data)
drop1(M3.full)
## drop aspect 
M3.full <- lmer(gap_100_200_diff ~ graze + sand + clay + ELEVATION + year + imazapic_binary +(1 | plot_num), REML = TRUE, data = data)
drop1(M3.full)
## drop graze
M3.full <- lmer(gap_100_200_diff ~ sand + clay + ELEVATION + year + imazapic_binary +(1 | plot_num), REML = TRUE, data = data)
drop1(M3.full)
## drop imazapic
M3.full <- lmer(gap_100_200_diff ~ sand + clay + ELEVATION + year + (1 | plot_num), REML = TRUE, data = data)
drop1(M3.full)
## drop year
M3.full <- lmer(gap_100_200_diff ~ sand + clay + ELEVATION + (1 | plot_num), REML = TRUE, data = data)
drop1(M3.full)
## drop clay
M3.full <- lmer(gap_100_200_diff ~ sand + ELEVATION + (1 | plot_num), REML = TRUE, data = data)
drop1(M3.full)
#LFB
gap_100_200_fb <- lmer(gap_100_200_diff ~ sand + ELEVATION + (1 | plot_num), REML = TRUE, data = data)
summary(gap_100_200_fb)
plot(resid(gap_100_200_fb, type = "deviance"))
abline(0,0)
r.squaredGLMM(gap_100_200_fb)




## calculate difference
gap <-data.frame()
gap <- cover_gap[ -c(5:15)]
gap <- gap[-c(6:23)]
gap <- gap[-c(3)]
gap_gt200_wide <- pivot_wider(gap, names_from = "plot_let", values_from = "gap_percent_gt200")
gap_gt200_wide$gap_gt200_diff <- gap_gt200_wide$G - gap_gt200_wide$A
gap_gt200_wide <- gap_gt200_wide %>% unite("plot_line", c("plot_num","LineKey"), sep = "_", remove = FALSE)
#attach fixed variables to ht_wide
data <- data.frame()
data <-merge(gap_gt200_wide, cover_gap_sub, by = "plot_line")
## gap_percent_gt200
M3.full <- lmer(gap_gt200_diff ~ aspect_card + graze + sand + clay + ELEVATION + year + imazapic_binary +(1 | plot_num), REML = TRUE, data = data)
drop1(M3.full)
## drop clay
M3.full <- lmer(gap_gt200_diff ~ aspect_card + graze + sand + ELEVATION + year + imazapic_binary +(1 | plot_num), REML = TRUE, data = data)
drop1(M3.full)
## drop sand
M3.full <- lmer(gap_gt200_diff ~ aspect_card + graze + ELEVATION + year + imazapic_binary +(1 | plot_num), REML = TRUE, data = data)
drop1(M3.full)
## drop imazapic
M3.full <- lmer(gap_gt200_diff ~ aspect_card + graze + ELEVATION + year + (1 | plot_num), REML = TRUE, data = data)
drop1(M3.full)
## drop elevation
M3.full <- lmer(gap_gt200_diff ~ aspect_card + graze + year + (1 | plot_num), REML = TRUE, data = data)
drop1(M3.full)
## drop aspect
M3.full <- lmer(gap_gt200_diff ~ graze + year + (1 | plot_num), REML = TRUE, data = data)
drop1(M3.full)
#LFB
gap_gt200_fb <- lmer(gap_gt200_diff ~ graze + year + (1 | plot_num), REML = TRUE, data = data)
summary(gap_gt200_fb)
plot(resid(gap_gt200_fb, type = "deviance"))
abline(0,0)
r.squaredGLMM(gap_gt200_fb)




## calculate difference
gap <-data.frame()
gap <- cover_gap[ -c(5:10)]
gap <- gap[-c(6:28)]
gap <- gap[-c(3)]
gap_perc_wide <- pivot_wider(gap, names_from = "plot_let", values_from = "gap_percent")
gap_perc_wide$gap_perc_diff <- gap_perc_wide$G - gap_perc_wide$A
gap_perc_wide <- gap_perc_wide %>% unite("plot_line", c("plot_num","LineKey"), sep = "_", remove = FALSE)
#attach fixed variables to ht_wide
data <- data.frame()
data <-merge(gap_perc_wide, cover_gap_sub, by = "plot_line")
## gap_percent
M3.full <- lmer(gap_perc_diff ~ aspect_card + graze + sand + clay + ELEVATION + year + imazapic_binary +(1 | plot_num), REML = TRUE, data = data)
drop1(M3.full)
## drop aspect
M3.full <- lmer(gap_perc_diff ~ graze + sand + clay + ELEVATION + year + imazapic_binary +(1 | plot_num), REML = TRUE, data = data)
drop1(M3.full)
## drop year
M3.full <- lmer(gap_perc_diff ~ graze + sand + clay + ELEVATION + imazapic_binary +(1 | plot_num), REML = TRUE, data = data)
drop1(M3.full)
## drop clay
M3.full <- lmer(gap_perc_diff ~ graze + sand + ELEVATION + imazapic_binary +(1 | plot_num), REML = TRUE, data = data)
drop1(M3.full)
## drop imazapic
M3.full <- lmer(gap_perc_diff ~ graze + sand + ELEVATION + (1 | plot_num), REML = TRUE, data = data)
drop1(M3.full)
## drop elevation
M3.full <- lmer(gap_perc_diff ~ graze + sand + (1 | plot_num), REML = TRUE, data = data)
drop1(M3.full)
## drop sand
M3.full <- lmer(gap_perc_diff ~ graze + (1 | plot_num), REML = TRUE, data = data)
drop1(M3.full)
#LFB
gap_perc_fb <- lmer(gap_perc_diff ~ graze + (1 | plot_num), REML = TRUE, data = data)
summary(gap_perc_fb)
plot(resid(gap_perc_fb, type = "deviance"))
abline(0,0)
r.squaredGLMM(gap_perc_fb)
