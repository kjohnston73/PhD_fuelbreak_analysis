manova_data <- read.csv(file = "data/manova_data.csv")

# for (i in 3:24) {
#   qqnorm(manova_data[,i], ylab="Ordered Observations", main = paste0(colnames(manova_data)[i]))
#   qqline(manova_data[,i])
# }

res.man <- manova(cbind(load_3root, IAG, SEEDED, WEED, gap_mean_log,
                        gap_percent, gap_percent_lt25, gap_percent_25_50, gap_percent_51_100,
                        gap_percent_101_200, gap_percent_gt200, ht_sqrt) ~ graze, data = manova_data)
summary(res.man)

summary.aov(res.man)
