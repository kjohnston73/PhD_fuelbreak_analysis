# import libraries
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
load <-data.frame()
load <- ht_load[, c("plot_num", "plot_let", "LineKey", "herbload")]
load_wide <- pivot_wider(load, names_from = "plot_let", values_from = "herbload")
load_wide$load_diff <- load_wide$G - load_wide$A
load_wide <- load_wide %>% unite("plot_line", c("plot_num","LineKey"), sep = "_", remove = FALSE)

## subset data to not grazed and not grazed adjacent
ht_load_sub <- subset(ht_load, graze =='T' | graze == 'A'| graze == 'N')
ht_load_sub <- ht_load_sub %>% unite("plot_line", c("plot_num","LineKey"), sep = "_", remove = FALSE)
ht_load_sub <- ht_load_sub[ -c(2:21) ]
#attach fixed variables to ht_wide
data <- data.frame()
data <-merge(load_wide, ht_load_sub, by = "plot_line")


# GLMM process
# Step 1 - linear regression
M1 <- lm(load_diff ~ aspect_card + graze + sand + clay + ELEVATION + year + imazapic_binary, 
         data = data)
E1 <- rstandard(M1)
plot(E1, ylab = "standardized residuals")
abline(0,0)
plot_histogram(E1)
summary(M1)
AIC(M1) # 4068.025

# Step 2 - fit the gls
M2 <- gls(load_diff ~ aspect_card + graze + sand + clay + ELEVATION + year + imazapic_binary, 
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
M3 <- lmer(load_diff ~ aspect_card + graze + sand + clay + ELEVATION + year + imazapic_binary + (1 | plot_num), REML = TRUE, data = data)
## compare gls to random-intercept lme
anova(M2,M3) # M3 AIC 1191.468 - lower than M2
summary(M3)

## no variable makes sense to use as a random slope, skip

## Step 6 - check the residuals, add variance structure if needed
plot(M3, col = (data$imazapic_binary))#, cex=(data$graze)
## smaller variance for smaller fitted values, no clear correlation to any fixed variable
m3_res<-resid(M3, type = "deviance")
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
M3.full <- lmer(load_diff ~ aspect_card + graze + sand + clay + ELEVATION + year + imazapic_binary + (1 | plot_num), REML = TRUE, data = data)
drop1(M3.full)
#drop aspect
M3.full <- lmer(load_diff ~ graze + sand + clay + ELEVATION + year + imazapic_binary + (1 | plot_num), REML = TRUE, data = data)
drop1(M3.full)
#drop graze
M3.full <- lmer(load_diff ~ sand + clay + ELEVATION + year + imazapic_binary + (1 | plot_num), REML = TRUE, data = data)
drop1(M3.full)
#drop elevation
M3.full <- lmer(load_diff ~ sand + clay + year + imazapic_binary + (1 | plot_num), REML = TRUE, data = data)
drop1(M3.full)
#drop imazapic
M3.full <- lmer(load_diff ~ sand + clay +  year + (1 | plot_num), REML = TRUE, data = data)
drop1(M3.full)
#drop sand
M3.full <- lmer(load_diff ~ clay +year + (1 | plot_num), REML = TRUE, data = data)
drop1(M3.full)


#LFB
load_fb <- lmer(load_diff ~ clay +year + (1 | plot_num), REML = TRUE, data = data)
summary(load_fb)
plot(resid(load_fb, type = "deviance"))
abline(0,0)
r.squaredGLMM(load_fb)

### scatter Plot with clay
p <- ggplot(data, aes(x=clay, y=load_diff, colour =year)) +
  theme_classic() +
  theme(axis.text=element_text(size=20,face="bold"), axis.title=element_text(size=20,face="bold")) +
  geom_point() +
  ggtitle("") +
  xlab("Surface Soil Clay (%)") +
  ylab("Difference in Herbaceous Load (kg/ha)") +
  labs(fill = "Year") +
  geom_smooth(method=lm) +
  theme_bw(base_size=22) 
p


### 
load_lme <- lme(load_diff ~ clay +year, random = ~1 | plot_num, method = "ML", data = data)
newdat <- expand.grid(year=unique(data$year),
                      clay=c(min(data$clay),
                             max(data$clay)))

gr <- ref_grid(load_lme, cov.keep= c('clay', 'year'))
emm <- emmeans(gr, spec= c('clay', 'year'), level= 0.95)

p <- ggplot(data, aes(x=clay, y=load_diff, colour=year)) +
  # geom_point(size=3) +
  theme_classic() +
  theme(axis.text=element_text(size=20,face="bold"), axis.title=element_text(size=20,face="bold")) +
  geom_ribbon(data= data.frame(emm), aes(ymin= lower.CL, ymax= upper.CL, y= NULL), fill= 'grey80') +
  geom_line(data=newdat, aes(y=predict(load_lme, level=0, newdata=newdat)), size = 2) +
  xlab("Surface Soil Clay (Proportion)") +
  ylab("diff in herbaceous load") +
  labs(fill = "Year") +
  theme_bw(base_size=22) 
p

# diff plot
# Calculates mean, sd, se and 95% CI
load3 <- data %>%
  summarise( 
    n=n(),
    mean=mean(load_diff),
    sd=sd(load_diff)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1))
load3$year <- "overall"

load4 <- data %>%
  group_by(year) %>%
  summarise( 
    n=n(),
    mean=mean(load_diff),
    sd=sd(load_diff)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1))

load5<- rbind(load3, load4)
#plot
load5 %>%
  ggplot(aes(x=year,y = mean)) + 
  theme_minimal() +
  theme(axis.text=element_text(size=16, face="bold"), axis.title=element_text(size=16,face="bold")) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.3, colour="darkcyan", alpha=1, size=2) +
  geom_hline(yintercept=0) +
  geom_point(size = 6, colour = "darkcyan") +
  scale_x_discrete(labels = c("2021", "2022", "Overall")) +
  labs(x = NULL, y = "Difference in Herbaceous Load (kg/ha)") +
  theme(legend.position = "none") 
