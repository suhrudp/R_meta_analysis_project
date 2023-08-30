# set working directory
setwd("your/working/directory")

# load libraries
library(tidyverse)
library(metafor)
library(meta)
library(pimeta)
library(metaviz)

# import data
df <- read.csv("your/working/directory/data.csv", 
               check.names = FALSE)
attach(df)
df %>% colnames()

# calculate the effect size (log odds) for a binary outcome (development of microvascular complications)
dfes <- escalc(measure = "OR", 
               ai = tpos, 
               bi = tneg, 
               ci = cpos, 
               di = cneg, 
               data = df)
dfes$sei <- sqrt(dfes$vi) # calculate the standard error from the variance
head(dfes)

# fit a model using a linear mixed effects approach
ma.mod <- rma(yi = yi, 
              sei = sei, 
              slab = Author..Year., 
              data = dfes)

summary(ma.mod)

# get prediction intervals (due to high heterogeneity)
bootPI(dfes$yi, dfes$sei)

# calculate studentized residuals
rstudent(ma.mod)

# calculate cook's distances
cooks.distance(ma.mod)

# plot a rainforest plot 
forest <- viz_forest(x = dfes[, c("yi", "sei")], 
                    study_labels = dfes[, "Author"], 
                    xlab = "Log Odds",
                    variant = "rain",
                    method = "DL",
                    confidence_level = 0.95,
                    text_size = 5,annotate_CI = TRUE,
                    summary_col = "Reds")

# save the plot in high quality
ggsave(plot = forest,
       filename = "Forest Plot.png",
       height = 8,
       width = 16,
       dpi = 600)

# plot a funnel plot
funnel <- viz_funnel(x = dfes[, c("yi", "sei")],
                     method = "DL",
                     sig_contours = F, 
                     text_size = 5, 
                     point_size = 3,
                     egger = T,
                     xlab = "Log Odds")

# regression test for funnel plot asymmetry
regtest(ma.mod)

# save the plot in high quality
ggsave(plot = funnel,
       filename = "Funnel Plot.png",
       height = 10,
       width = 14,
       dpi = 600)

# sensitivity analysis
dfessens <- dfes[-c(1,4),]
ma.mod1 <- rma(yi = yi, 
              sei = sei, 
              slab = Author..Year., 
              data = dfessens)

summary(ma.mod1)

bootPI(dfessens$yi, dfessens$sei)

forestsens <- viz_forest(x = dfessens[, c("yi", "sei")], 
                     study_labels = dfessens[, "Author"], 
                     xlab = "Log Odds",
                     variant = "rain",
                     method = "DL",
                     confidence_level = 0.95,
                     text_size = 5,annotate_CI = TRUE,
                     summary_col = "Reds")

ggsave(plot = forestsens,
       filename = "Forest Plot - Sensitivity.png",
       height = 8,
       width = 16,
       dpi = 600)

funnelsens <- viz_funnel(x = dfessens[, c("yi", "sei")],
                     method = "DL",
                     sig_contours = F, 
                     text_size = 5, 
                     point_size = 3,
                     egger = T,
                     xlab = "Log Odds")

regtest(ma.mod1)

ggsave(plot = funnelsens,
       filename = "Funnel Plot - Sensitivity.png",
       height = 10,
       width = 14,
       dpi = 600)
