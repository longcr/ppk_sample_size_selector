# explore regression model
# cliff Long
# 2026-05-26

# load packages ---------------------------------------------------------------

library(here)
library(readxl)
library(janitor)
library(dplyr)
library(ggplot2)
library(ggeffects)
library(RColorBrewer)
library(plotly)



# load data -------------------------------------------------------------------

fname <- "SCMH.xlsx"
fsheet <- "table_long"

d0 <- read_excel(path = fname, sheet = fsheet)

glimpse(d0)


d1 <- d0 %>% 
  mutate(lcl90_meets_fct = factor(lcl90_meets))

glimpse(d1)




# plot original data ----------------------------------------------------------

d1 %>% 
  ggplot(aes(x = ssize, y = req_calc_pointest, color = lcl90_meets_fct)) + 
  geom_point() + 
  geom_line() + 
  NULL




# model data ------------------------------------------------------------------

fit1 <- lm(log(req_calc_pointest) ~ lcl90_meets_fct + log(ssize), data = d1)

summary(fit1)



# plot based on model ---------------------------------------------------------


# Interpolate 11 colors using a pre-existing Brewer palette (e.g., "Spectral" or "Set3")
my_colors <- colorRampPalette(brewer.pal(11, "Spectral"))(11)


# generate predicated values including extrapolation of ssize from 250 to 500

## prediction within domain of the data ssize up to 250
preds_domain <- ggpredict(fit1, 
                          terms = c("ssize [20:250, by = 5]", "lcl90_meets_fct")) 


## with extrapolation from ssize 250 to 500
preds_extrap <- ggpredict(fit1, 
                          terms = c("ssize [20:500, by = 10]", "lcl90_meets_fct")) 



## select only one
preds <- preds_domain
preds <- preds_extrap


## create plot
plot1 <- plot(preds) + 
  geom_point(size = 0.1, stroke = 0) + 
  scale_color_manual(values = my_colors) + 
  ggtitle(label = "Required Calculated Point Estimate for Ppk") + 
  ylab('Calculated Point Estimate of Ppk') + 
  xlab('Sample Size') + 
  NULL


## if-then

# str(preds_domain)
# preds_domain$x

if (max(preds$x) == 250) {
  plot1 <- plot1
} else {
  plot1 <- plot1 + 
    annotate("rect", 
             xmin = 250, xmax = max(preds$x), 
             ymin = -Inf, ymax = Inf, 
             fill = "grey", alpha = 0.2)
}


plot1

plotly::ggplotly(plot1)





# end code --------------------------------------------------------------------