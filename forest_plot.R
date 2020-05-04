library(ggplot2)
library(tidyverse)

sen_data <- read.csv("sensitivity_data.csv")
sp_data <- read.csv("specificity_data.csv")

# This step is to control the order of labels: the default order is based on the alphabet of Test names, now it is changed to the same as the csv file
## Turn the "order" column into a character vector
sen_data$Test <- as.character(sen_data$Test)
## Then turn it back into a factor with the levels in the correct order (now I set it to the same as the csv order)
sen_data$Test <- factor(sen_data$Test, levels = rev(unique(sen_data$Test)))

# Version 1: labels are testing methods
## transform() is used to control the order of 4 subgroups
p1 <- ggplot(data = transform(sen_data, 
                              Group = factor(Group, levels = c("Gross macroscopic findings", "Thickness", "Nodularity", "Other findings"))), 
             aes(x = Test, y = Sensitivity, ymin = sn_lower, ymax = sn_upper)) + 
  geom_pointrange(aes(col = Study), size = 0.2) + 
  xlab('Test') + ylab("Sensitivity (95% Confidence Interval)") +
  geom_errorbar(aes(ymin = sn_lower, ymax = sn_upper, col = Study), width = 0.5, cex = 0.5) + 
  ## facet_wrap() is used to create the 4 subgroups
  facet_wrap( ~ Group, strip.position = "top", nrow = 4, scales = "free_y") +
  theme(plot.title = element_text(size = 16, face = "bold"), 
        axis.ticks.y = element_blank(), 
        axis.text.x = element_text(face="bold"), axis.title = element_text(size = 12, face = "bold")) +
  ## scale_x_discrete() is used to change the labels from the Test (study name + method) to only method
  coord_flip() + scale_x_discrete(breaks = sen_data$Test, labels = sen_data$Method) 

# Change the height of the row in each panel
## convert ggplot object to grob object
gp1 <- ggplotGrob(p1)
## optional: take a look at the grob object's layout
gtable::gtable_show_layout(gp1)
## get gtable columns corresponding to the facets: t-top, b-bottom, l-left, r-right, we get the top location for each row
facet.rows1 <- gp1$layout$t[grepl("panel", gp1$layout$name)]
## get the number of unique x-axis values per facet: the distance between the top of 2 panels
x1.var <- sapply(ggplot_build(p1)$layout$panel_scales_x,
                 function(t) length(t$range$range))
## change the relative heights of the facet columns based on how many unique x-axis values are in each facet
gp1$heights[facet.rows1] <- gp1$heights[facet.rows1] * x1.var
## plot result
grid::grid.draw(gp1)

# Version 2: labels are study names + testing methods
# p2 <- ggplot(data = transform(sen_data, 
#                              Group = factor(Group, levels = c("Gross macroscopic findings", "Thickness", "Nodularity", "Other findings"))), 
#              aes(x = Test, y = Sensitivity, ymin = sn_lower, ymax = sn_upper, label = Method)) + 
#         geom_pointrange(aes(col = Study), size = 0.2) + 
#         xlab('Test') + ylab("Sensitivity (95% Confidence Interval)") +
#         geom_errorbar(aes(ymin = sn_lower, ymax = sn_upper, col = Study), width = 0.5, cex = 0.5) + 
#         facet_wrap( ~ Group, strip.position = "top", nrow = 4, scales = "free_y") +
#         theme(plot.title = element_text(size = 16, face = "bold"), 
#               axis.ticks.y = element_blank(), 
#               axis.text.x = element_text(face="bold"), axis.title = element_text(size = 12, face = "bold")) +
#         coord_flip()


# For specificity:
sp_data$Test <- as.character(sp_data$Test)
sp_data$Test <- factor(sp_data$Test, levels = rev(unique(sp_data$Test)))

p3 <- ggplot(data = transform(sp_data, 
                              Group = factor(Group, levels = c("Gross macroscopic findings", "Thickness", "Nodularity", "Other findings"))), 
             aes(x = Test, y = Specificity, ymin = sp_lower, ymax = sp_upper)) + 
  geom_pointrange(aes(col = Study), size = 0.2) + 
  xlab('Test') + ylab("Specificity (95% Confidence Interval)") +
  geom_errorbar(aes(ymin = sp_lower, ymax = sp_upper, col = Study), width = 0.5, cex = 0.5) + 
  ## facet_wrap() is used to create the 4 subgroups
  facet_wrap( ~ Group, strip.position = "top", nrow = 4, scales = "free_y") +
  theme(plot.title = element_text(size = 16, face = "bold"), 
        axis.ticks.y = element_blank(), 
        axis.text.x = element_text(face="bold"), axis.title = element_text(size = 12, face = "bold")) +
  ## scale_x_discrete() is used to change the labels from the Test (study name + method) to only method
  coord_flip() + scale_x_discrete(breaks = sp_data$Test, labels = sp_data$Method) 

# Change the height of the row in each panel
gp3 <- ggplotGrob(p3)
## optional: take a look at the grob object's layout
gtable::gtable_show_layout(gp3)
## get gtable columns corresponding to the facets: t-top, b-bottom, l-left, r-right, we get the top location for each row
facet.rows3 <- gp3$layout$t[grepl("panel", gp3$layout$name)]
## get the number of unique x-axis values per facet: the distance between the top of 2 panels
x3.var <- sapply(ggplot_build(p3)$layout$panel_scales_x,
                 function(t) length(t$range$range))
## change the relative heights of the facet columns based on how many unique x-axis values are in each facet
gp3$heights[facet.rows3] <- gp3$heights[facet.rows3] * x3.var
## plot result
grid::grid.draw(gp3)