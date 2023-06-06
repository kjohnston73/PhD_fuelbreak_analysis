library(ggplot2)
library(dplyr)
library(forcats)
library(tidyr)
library(gridExtra)

cover_gap <- read.csv(file = "data/glmm_cover_gap_data.csv")
ht_load <- read.csv(file = "data/glmm_ht_load_data.csv")

# Calculates mean, sd, se and 95% CI
iag <- cover_gap %>%
  group_by(graze) %>%
  summarise( 
    n=n(),
    mean=mean(IAG),
    sd=sd(IAG)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1))

# plot
iag %>%
  mutate(graze = factor(graze, levels=c("A", "AA", "N", "AN", "T", "AT"))) %>%
  ggplot( aes(x=graze, y=mean, fill =graze)) +
  theme_minimal() +
  theme(axis.text=element_text(size=16), axis.title=element_text(size=16,face="bold")) +
  geom_bar( aes(x=graze, y=mean), stat="identity", alpha=1) +
  scale_fill_manual(values=c("orchid", "purple", "lightgreen", "seagreen", "salmon", "firebrick")) +
  geom_errorbar( aes(x=graze, ymin=mean-se, ymax=mean+se), width=0.2, colour="black", alpha=1, size=1) +
  scale_x_discrete(labels = c("Allotment", "Allotment Adjacent","Not Grazed", "Not Grazed Adjacent","Targeted", "Targeted Adjacent")) +
  labs(x = NULL, y = "Invasive Annual Grass Cover (%)") +
  ylim(0,35) +
  theme(legend.position = "none") 


# Calculates mean, sd, se and 95% CI
weed <- cover_gap %>%
  group_by(graze) %>%
  summarise( 
    n=n(),
    mean=mean(WEED),
    sd=sd(WEED)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1))


# Standard Error
weed %>%
  mutate(graze = factor(graze, levels=c("A", "AA", "N", "AN", "T", "AT"))) %>%
  ggplot( aes(x=graze, y=mean, fill =graze)) +
  theme_minimal() +
  theme(axis.text=element_text(size=16), axis.title=element_text(size=16,face="bold")) +
  geom_bar( aes(x=graze, y=mean), stat="identity", alpha=1) +
  scale_fill_manual(values=c("orchid", "purple", "lightgreen", "seagreen", "salmon", "firebrick")) +
  geom_errorbar( aes(x=graze, ymin=mean-se, ymax=mean+se), width=0.2, colour="black", alpha=1, size=1) +
  scale_x_discrete(labels = c("Allotment", "Allotment Adjacent", "Not Grazed", "Not Grazed Adjacent", "Targeted", "Targeted Adjacent")) +
  labs(x = NULL, y = "Other Weed Cover (%)") +
  theme(legend.position = "none") 



# Calculates mean, sd, se and 95% CI
seeds <- subset(cover_gap, graze =='A' | graze == 'N')
seed <- seeds %>%
  group_by(graze) %>%
  summarise( 
    n=n(),
    mean=mean(SEEDED),
    sd=sd(SEEDED)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1))


# Standard Error
seed %>%
  mutate(graze = factor(graze, levels=c("A", "N"))) %>%
  ggplot( aes(x=graze, y=mean, fill =graze)) +
  theme_minimal() +
  theme(axis.text=element_text(size=16), axis.title=element_text(size=16,face="bold")) +
  geom_bar( aes(x=graze, y=mean), stat="identity", alpha=1) +
  scale_fill_manual(values=c("orchid", "lightgreen")) +
  geom_errorbar( aes(x=graze, ymin=mean-se, ymax=mean+se), width=0.2, colour="black", alpha=1, size=1) +
  scale_x_discrete(labels = c("Allotment", "Not Grazed")) +
  labs(x = NULL, y = "Seeded Species Cover (%)") +
  theme(legend.position = "none") 



# Calculates mean, sd, se and 95% CI
gap <- cover_gap %>%
  group_by(graze) %>%
  summarise( 
    n=n(),
    mean=mean(gap_mean),
    sd=sd(gap_mean)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1))


# Standard Error
gap %>%
  mutate(graze = factor(graze, levels=c("A", "AA", "N", "AN", "T", "AT"))) %>%
  ggplot( aes(x=graze, y=mean, fill =graze)) +
  theme_minimal() +
  theme(axis.text=element_text(size=16), axis.title=element_text(size=16,face="bold")) +
  geom_bar( aes(x=graze, y=mean), stat="identity", alpha=1) +
  scale_fill_manual(values=c("orchid", "purple", "lightgreen", "seagreen", "salmon", "firebrick")) +
  geom_errorbar( aes(x=graze, ymin=mean-se, ymax=mean+se), width=0.2, colour="black", alpha=1, size=1) +
  scale_x_discrete(labels = c("Allotment", "Allotment Adjacent", "Not Grazed", "Not Grazed Adjacent", "Targeted", "Targeted Adjacent")) +
  labs(x = NULL, y = "Mean Gap Size (cm)") +
  theme(legend.position = "none") 


# Calculates mean, sd, se and 95% CI
gap_perc <- cover_gap %>%
  group_by(graze) %>%
  summarise( 
    n=n(),
    mean=mean(gap_percent),
    sd=sd(gap_percent)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1))


# Standard Error
gap_perc %>%
  mutate(graze = factor(graze, levels=c("A", "AA", "N", "AN", "T", "AT"))) %>%
  ggplot( aes(x=graze, y=mean, fill =graze)) +
  theme_minimal() +
  theme(axis.text=element_text(size=16), axis.title=element_text(size=16,face="bold")) +
  geom_bar( aes(x=graze, y=mean), stat="identity", alpha=1) +
  scale_fill_manual(values=c("orchid", "purple", "lightgreen", "seagreen", "salmon", "firebrick")) +
  geom_errorbar( aes(x=graze, ymin=mean-se, ymax=mean+se), width=0.2, colour="black", alpha=1, size=1) +
  scale_x_discrete(labels = c("Allotment", "Allotment Adjacent", "Not Grazed", "Not Grazed Adjacent", "Targeted", "Targeted Adjacent")) +
  labs(x = NULL, y = "Total Percent Gap") +
  theme(legend.position = "none") 


# Calculates mean, sd, se and 95% CI
ht <- ht_load %>%
  group_by(graze) %>%
  summarise( 
    n=n(),
    mean=mean(max_height),
    sd=sd(max_height)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1))


# Standard Error
ht %>%
  mutate(graze = factor(graze, levels=c("A", "AA", "N", "AN", "T", "AT"))) %>%
  ggplot( aes(x=graze, y=mean, fill =graze)) +
  theme_minimal() +
  theme(axis.text=element_text(size=16), axis.title=element_text(size=16,face="bold")) +
  geom_bar( aes(x=graze, y=mean), stat="identity", alpha=1) +
  scale_fill_manual(values=c("orchid", "purple", "lightgreen", "seagreen", "salmon", "firebrick")) +
  geom_errorbar( aes(x=graze, ymin=mean-se, ymax=mean+se), width=0.2, colour="black", alpha=1, size=1) +
  scale_x_discrete(labels = c("Allotment", "Allotment Adjacent", "Not Grazed", "Not Grazed Adjacent", "Targeted", "Targeted Adjacent")) +
  labs(x = NULL, y = "Average Maximum Height (cm)") +
  theme(legend.position = "none") 



# Calculates mean, sd, se and 95% CI
load <- ht_load %>%
  group_by(graze) %>%
  summarise( 
    n=n(),
    mean=mean(herbload),
    sd=sd(herbload)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1))


# Standard Error
load %>%
  mutate(graze = factor(graze, levels=c("A", "AA", "N", "AN", "T", "AT"))) %>%
  ggplot( aes(x=graze, y=mean, fill =graze)) +
  theme_minimal() +
  theme(axis.text=element_text(size=16), axis.title=element_text(size=16,face="bold")) +
  geom_bar( aes(x=graze, y=mean), stat="identity", alpha=1) +
  scale_fill_manual(values=c("orchid", "purple", "lightgreen", "seagreen", "salmon", "firebrick")) +
  geom_errorbar( aes(x=graze, ymin=mean-se, ymax=mean+se), width=0.2, colour="black", alpha=1, size=1) +
  scale_x_discrete(labels = c("Allotment", "Allotment Adjacent", "Not Grazed", "Not Grazed Adjacent", "Targeted", "Targeted Adjacent")) +
  labs(x = NULL, y = "Herbaceous Fuel Loading (kg/ha)") +
  theme(legend.position = "none") 




# gaps Grouped
# Calculates mean, sd, se and 95% CI
gap_lt25 <- cover_gap %>%
  group_by(graze) %>%
  summarise( 
    n=n(),
    mean=mean(gap_percent_lt25),
    sd=sd(gap_percent_lt25)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1))

gap_lt25$type <- "<25"

gap_25_50 <- cover_gap %>%
  group_by(graze) %>%
  summarise( 
    n=n(),
    mean=mean(gap_percent_25_50),
    sd=sd(gap_percent_25_50)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1))

gap_25_50$type <- "25-50"

gap_50_100 <- cover_gap %>%
  group_by(graze) %>%
  summarise( 
    n=n(),
    mean=mean(gap_percent_51_100),
    sd=sd(gap_percent_51_100)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1))

gap_50_100$type <- "51-100"

gap_100_200 <- cover_gap %>%
  group_by(graze) %>%
  summarise( 
    n=n(),
    mean=mean(gap_percent_101_200),
    sd=sd(gap_percent_101_200)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1))

gap_100_200$type <- "101-200"

gap_gt200 <- cover_gap %>%
  group_by(graze) %>%
  summarise( 
    n=n(),
    mean=mean(gap_percent_gt200),
    sd=sd(gap_percent_gt200)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1))

gap_gt200$type <- ">200"

gaps<- rbind(gap_lt25, gap_25_50, gap_50_100, gap_100_200, gap_gt200)

par(mfrow = c(3,1))
gaps_A <-subset(gaps, graze =='A' | graze == 'AA')
gaps_N <-subset(gaps, graze =='N' | graze == 'AN')
gaps_T <-subset(gaps, graze =='T' | graze == 'AT')
plota<- gaps_A %>%
  mutate(graze = factor(graze, levels=c("A", "AA"))) %>%
  mutate(type = factor(type, levels=c("<25", "25-50", "51-100", "101-200", ">200"))) %>%
  ggplot(aes(x=graze, y=mean, fill=type)) +
  theme_minimal() +
  theme(axis.text=element_text(size=16), axis.title=element_text(size=16,face="bold")) +
  geom_bar(aes(x=graze, y=mean), position="dodge", stat="identity", alpha=1) +
  scale_fill_viridis_d() +
  ylim(0,6) +
  theme(legend.position="top") +
  guides(fill=guide_legend(title="Gap Size (cm)")) +
  geom_errorbar(aes(x=graze, ymin=mean-se, ymax=mean+se), width=0.2, colour="black", alpha=1, size=1, position=position_dodge((0.9))) +
  labs(x = NULL, y = "") +
  scale_x_discrete(labels = c("Allotment", "Allotment Adjacent"))


plotn<-gaps_N %>%
  mutate(graze = factor(graze, levels=c("N", "AN"))) %>%
  mutate(type = factor(type, levels=c("<25", "25-50", "51-100", "101-200", ">200"))) %>%
  ggplot(aes(x=graze, y=mean, fill=type)) +
  theme_minimal() +
  theme(axis.text=element_text(size=16), axis.title=element_text(size=16,face="bold")) +
  geom_bar(aes(x=graze, y=mean), position="dodge", stat="identity", alpha=1) +
  scale_fill_viridis_d() +
  ylim(0,6) +
  theme(legend.position = "none") +
  geom_errorbar(aes(x=graze, ymin=mean-se, ymax=mean+se), width=0.2, colour="black", alpha=1, size=1, position=position_dodge((0.9))) +
  labs(x = NULL, y = "Percent (%)") +
  scale_x_discrete(labels = c("Not Grazed", "Not Grazed Adjacent"))

plott<-gaps_T %>%
  mutate(graze = factor(graze, levels=c("T", "AT"))) %>%
  mutate(type = factor(type, levels=c("<25", "25-50", "51-100", "101-200", ">200"))) %>%
  ggplot(aes(x=graze, y=mean, fill=type)) +
  theme_minimal() +
  theme(axis.text=element_text(size=16), axis.title=element_text(size=16,face="bold")) +
  geom_bar(aes(x=graze, y=mean), position="dodge", stat="identity", alpha=1) +
  scale_fill_viridis_d() +
  ylim(0,6) +
  theme(legend.position = "none") +
  geom_errorbar(aes(x=graze, ymin=mean-se, ymax=mean+se), width=0.2, colour="black", alpha=1, size=1, position=position_dodge((0.9))) +
  labs(x = NULL, y = "") +
  scale_x_discrete(labels = c("Targeted", "Targeted Adjacent"))

grid.arrange(plota, plotn, plott, nrow = 3)

