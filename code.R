rm(list=ls())
cat('\014')

if (!require(haven)) install.packages("haven"); library(haven)
if (!require(ggplot2)) install.packages("ggplot2"); library(ggplot2)
if (!require(statar)) install.packages("statar"); library(statar)

setwd("~/Desktop")
atlas <- read_dta("atlas.dta")
  
tract_a <- subset(atlas, state == 36 & county == 61 & tract == 15300) 
tract_b <- subset(atlas, state == 36 & county == 61 & tract == 15100) 
  
tract_a$share_white2000
tract_b$share_white2000

tract_a$share_black2000
tract_b$share_black2000

tract_a$share_hisp2000
tract_b$share_hisp2000

tract_a$share_asian2000
tract_b$share_asian2000

tract_a$poor_share2000
tract_b$poor_share2000
  
tract_a$singleparent_share2000
tract_b$singleparent_share2000

manhattan <- subset(atlas, state == 36 & county == 61)
  
ggplot(manhattan, aes(x = poor_share2000, y = kfr_pooled_pooled_p25)) +
  stat_binmean(n = 20) +
  stat_smooth(method = "lm", se = FALSE) +
  labs(x = "Poverty Rate 2000", y = "Absolute Mobility at the 25th Percentile")
ggsave("manhattan_binscatter_poor_share2000.png")

cor(manhattan$kfr_pooled_pooled_p25, manhattan$poor_share2000, use="pairwise.complete.obs")
summary(lm(kfr_pooled_pooled_p25 ~ poor_share2000, data=manhattan))
  
manhattan$share_nonwhite2000 <- manhattan$share_black2000 + manhattan$share_hisp2000 + manhattan$share_asian2000

ggplot(manhattan, aes(x = share_nonwhite2000, y = kfr_pooled_pooled_p25)) +
  stat_binmean(n = 20) +
  stat_smooth(method = "lm", se = FALSE) +
  labs(x = "Share Non-White 2000", y = "Absolute Mobility at the 25th Percentile")
ggsave("manhattan_binscatter_share_nonwhite2000.png")

cor(manhattan$kfr_pooled_pooled_p25, manhattan$share_nonwhite2000, use="pairwise.complete.obs")
summary(lm(kfr_pooled_pooled_p25 ~ share_nonwhite2000, data=manhattan))

ggplot(manhattan, aes(x = singleparent_share2000, y = kfr_pooled_pooled_p25)) +
  stat_binmean(n = 20) +
  stat_smooth(method = "lm", se = FALSE) +
  labs(x = "Share of Single-Headed Households with Children 2000", y = "Absolute Mobility at the 25th Percentile")
ggsave("manhattan_binscatter_singleparent_share2000.png")

cor(manhattan$kfr_pooled_pooled_p25, manhattan$singleparent_share2000, use="pairwise.complete.obs")
summary(lm(kfr_pooled_pooled_p25 ~ singleparent_share2000, data=manhattan))
