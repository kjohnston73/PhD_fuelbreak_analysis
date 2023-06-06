library(ggplot2)
library(dplyr)
# library(forcats)
library(tidyr)
# library(gridExtra)

cover_gap <- read.csv(file = "data/glmm_cover_gap_data.csv")
ht_load <- read.csv(file = "data/glmm_ht_load_data.csv")
#########
cover_gap <- cover_gap %>% 
  separate(plot,  
           into = c("plot_num", "plot_let"),
           sep = 3)
cover_gap_sub <- subset(cover_gap, graze =='T' | graze == 'A'| graze == 'N')
cover_gap_sub <- cover_gap_sub %>% unite("plot_line", c("plot_num","LineKey"), sep = "_", remove = FALSE)
cover_gap_sub <- cover_gap_sub[ -c(2:18) ]
#########
ht_load <- ht_load %>% 
  separate(plot,  
           into = c("plot_num", "plot_let"),
           sep = 3)
ht_load_sub <- subset(ht_load, graze =='T' | graze == 'A'| graze == 'N')
ht_load_sub <- ht_load_sub %>% unite("plot_line", c("plot_num","LineKey"), sep = "_", remove = FALSE)
ht_load_sub <- ht_load_sub[ -c(2:21) ]

######################  IAG ##################################
# set up dataframe
iag <-data.frame()
iag <- cover_gap[, c("plot_num", "plot_let", "LineKey", "IAG")]
iag_wide <- pivot_wider(iag, names_from = "plot_let", values_from = "IAG")
iag_wide$iag_diff <- iag_wide$G - iag_wide$A
iag_wide <- iag_wide %>% unite("plot_line", c("plot_num","LineKey"), sep = "_", remove = FALSE)
iag2 <- data.frame()
iag2 <-merge(iag_wide, cover_gap_sub, by = "plot_line")
# Calculates mean, sd, se and 95% CI
iag3 <- iag2 %>%
  group_by(graze) %>%
  summarise( 
    n=n(),
    mean=mean(iag_diff),
    sd=sd(iag_diff)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1))
# plot
iag3 %>%
  ggplot(aes(x=graze, y=mean, fill=graze)) + 
  theme_minimal() +
  theme(axis.text=element_text(size=16, face="bold"), axis.title=element_text(size=16,face="bold")) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.3, colour=c("purple", "darkcyan", "dodgerblue"), alpha=1, size=2) +
  geom_hline(yintercept=0) +
  geom_point(size = 6, colour = c("purple", "darkcyan", "dodgerblue")) +
  scale_x_discrete(labels = c("Allotment", "Not Grazed", "Targeted")) +
  labs(x = NULL, y = "Difference in Percent Invasive Annual Grass Cover") +
  theme(legend.position = "none") 



######################  LOAD  ##################################
# set up dataframe
load <-data.frame()
load <- ht_load[, c("plot_num", "plot_let", "LineKey", "herbload")]
load_wide <- pivot_wider(load, names_from = "plot_let", values_from = "herbload")
load_wide$load_diff <- load_wide$G - load_wide$A
load_wide <- load_wide %>% unite("plot_line", c("plot_num","LineKey"), sep = "_", remove = FALSE)
load2 <- data.frame()
load2 <-merge(load_wide, ht_load_sub, by = "plot_line")
# Calculates mean, sd, se and 95% CI
load3 <- load2 %>%
  group_by(graze) %>%
  summarise( 
    n=n(),
    mean=mean(load_diff),
    sd=sd(load_diff)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1))
# plot
load3 %>%
  ggplot(aes(x=graze, y=mean, fill=graze)) + 
  theme_minimal() +
  theme(axis.text=element_text(size=16, face="bold"), axis.title=element_text(size=16,face="bold")) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.3, colour=c("purple", "darkcyan", "dodgerblue"), alpha=1, size=2) +
  geom_hline(yintercept=0) +
  geom_point(size = 6, colour = c("purple", "darkcyan", "dodgerblue")) +
  scale_x_discrete(labels = c("Allotment", "Not Grazed", "Targeted")) +
  labs(x = NULL, y = "Difference in Herbaceous Load (kg/ha)") +
  theme(legend.position = "none") 

  



