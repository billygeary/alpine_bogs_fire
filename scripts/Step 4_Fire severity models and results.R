
# Step 4 Analysis of fire Severity
library(lme4)
library(tidyverse)
library(ggplot2)
library(ggeffects)
library(cowplot)

sev.data = read.csv("data_clean/peatlands_severity_data_new.csv")

names(sev.data)

# Need to choose between Shrubby, Mean Annual Rainfall and Elevation due to correlation
# AREA, FF_max, Elevation, Slope, Rainall, ASPN, SevBin

peatlands.filtered = filter(sev.data, Geo_group %in% c("Bogong Unit", "Buffalo", "Cobberas Unit"))


# Check for correlations
df.cor = peatlands.filtered %>% ungroup() %>%
  dplyr::select(HECTARES, Mean_elevation, Mean_slope, Mean_annual_min_temp, rain.anom.2019,
                Mean_annual_max_temp, Mean_annual_rainfall, Aspect, ASPN, FF_max, PA_Ratio, PropTree, PropTreeBuffer) %>%
  cor()

corrplot::corrplot(df.cor)

boxplot(peatlands.filtered$Mean_elevation ~ peatlands.filtered$SHRUBBY)
boxplot(peatlands.filtered$Mean_annual_rainfall ~ peatlands.filtered$SHRUBBY)


## At least half of peatland burnt severely
m0 = glmer(SevCov ~ (1|Geo_group), family = 'binomial', data=peatlands.filtered)
m.shrubby = glmer(SevCov ~ SHRUBBY + (1|Geo_group), family = 'binomial', data=peatlands.filtered)
m.rain = glmer(SevCov ~ scale(Mean_annual_rainfall) + (1|Geo_group), family = 'binomial', data=peatlands.filtered)
m.elevation = glmer(SevCov ~ scale(Mean_elevation) + (1|Geo_group), family = 'binomial', data=peatlands.filtered)

AIC(m0, m.shrubby, m.rain, m.elevation) #Shrubby then rain is the best

m.global = glmer(SevCov ~ scale(AREA) + scale(PA_Ratio) + scale(PropTreeBuffer) + scale(Mean_elevation)+ scale(rain.anom.2019) +
                   scale(Mean_slope) + scale(ASPN) + scale(FF_max) + (1|Geo_group), family = 'binomial', data=peatlands.filtered)
#m.global = glmer(SevCov ~ scale(AREA) + scale(PA_Ratio) + scale(PropTreeBuffer) + SHRUBBY + 
#                   scale(Mean_slope) + scale(ASPN) + scale(FF_max) + (1|Geo_group), family = 'binomial', data=peatlands.filtered)

DHARMa::testDispersion(m.global) # Looks fine
performance::check_model(m.global)

summary(m.global)
performance::r2(m.global) # Good R2

coefs = data.frame(summary(m.global)$coefficients)
write.csv(coefs, "outputs/fsev_option1_coefs_update_new.csv")


##Plots 

plot.model = m.global

area.pred= ggeffect(plot.model, terms = c("AREA"), ci.lvl = 0.95)
area.plot= ggplot(area.pred) + 
  geom_ribbon(aes(x=x, ymin = conf.low, ymax = conf.high), alpha = 0.4) + 
  geom_line(aes(x=x, y = predicted)) + theme_cowplot() + 
  theme_cowplot() + ylab("Probability Burnt Severely") + xlab("Peatland Area")

pa.pred= ggeffect(plot.model, terms = c("PA_Ratio"), ci.lvl = 0.95)
pa.plot= ggplot(pa.pred) + 
  geom_ribbon(aes(x=x, ymin = conf.low, ymax = conf.high), alpha = 0.4) + 
  geom_line(aes(x=x, y = predicted)) + theme_cowplot() + 
  theme_cowplot() + ylab("Probability Burnt Severely") + xlab("Perimeter-Area Ratio")

pt.pred= ggeffect(plot.model, terms = c("PropTreeBuffer"), ci.lvl = 0.95)
pt.plot= ggplot(pt.pred) + 
  geom_ribbon(aes(x=x, ymin = conf.low, ymax = conf.high), alpha = 0.4) + 
  geom_line(aes(x=x, y = predicted)) + theme_cowplot() + 
  theme_cowplot() + ylab("Probability Burnt Severely") + xlab("Surrounding Tree Cover")

elev.pred= ggeffect(plot.model, terms = c("Mean_elevation"), ci.lvl = 0.95)
elev.plot= ggplot(elev.pred) + 
  geom_ribbon(aes(x=x, ymin = conf.low, ymax = conf.high), alpha = 0.4) + 
  geom_line(aes(x=x, y = predicted)) + theme_cowplot() + 
  theme_cowplot() + ylab("Probability Burnt Severely") + xlab("Mean Elevation (m)")

slope.pred= ggeffect(plot.model, terms = c("Mean_slope"), ci.lvl = 0.95)
slope.plot= ggplot(slope.pred) + 
  geom_ribbon(aes(x=x, ymin = conf.low, ymax = conf.high), alpha = 0.4) + 
  geom_line(aes(x=x, y = predicted)) + theme_cowplot() + 
  theme_cowplot() + ylab("Probability Burnt Severely") + xlab("Mean Slope")

aspn.pred= ggeffect(plot.model, terms = c("ASPN"), ci.lvl = 0.95)
aspn.plot= ggplot(aspn.pred) + 
  geom_ribbon(aes(x=x, ymin = conf.low, ymax = conf.high), alpha = 0.4) + 
  geom_line(aes(x=x, y = predicted)) + theme_cowplot() + 
  theme_cowplot() + ylab("Probability Burnt Severely") + xlab("ASPN")

ff.pred = ggeffect(plot.model, terms = c("FF_max"), ci.lvl = 0.95)
ff.plot= ggplot(ff.pred) + 
  geom_pointrange(aes(x = x, y = predicted, ymin = conf.low, ymax = conf.high)) + 
  theme_cowplot() + ylab("Probability Burnt Severely") + xlab("Times burnt since 1985")

rain.anom.pred = ggeffect(plot.model, terms = c("rain.anom.2019"), ci.lvl = 0.95)
rain.anom.plot= ggplot(rain.anom.pred) + 
  geom_ribbon(aes(x=x, ymin = conf.low, ymax = conf.high), alpha = 0.4) + 
  geom_line(aes(x=x, y = predicted)) + theme_cowplot() + 
  theme_cowplot() + ylab("Probability Burnt Severely") + xlab("Rainfall Percentile (2019)")


shrub.pred = ggeffect(m.shrubby, terms = c("SHRUBBY"), ci.lvl = 0.95)
shrub.plot= ggplot(shrub.pred) + 
     geom_pointrange(aes(x=x,y=predicted, ymin = conf.low, ymax = conf.high)) + 
     theme_cowplot() + ylab("Probability Burnt Severely") + xlab("Shrubbyness")


## Tie together into one
halfsev.plot.model = cowplot::plot_grid(
                   area.plot, 
                   pa.plot,
                   pt.plot,
                   elev.plot,
                   slope.plot,
                   aspn.plot,
                   rain.anom.plot,
                   ff.plot, 
                   nrow=2, labels = c("a)","b)","c)", "d)", "e)","f)", "g)", "h)")
                   )

ggsave(plot = halfsev.plot.model, filename="halfsevmodel_plot_new.pdf", path="outputs", device="pdf", 
       width=6,height=4,units="in",scale=2.3)

########
# 
# ## Any amount burnt severely
# m0 = glmer(SevBin ~ (1|Geo_group), family = 'binomial', data=peatlands.filtered)
# m.shrubby = glmer(SevBin ~ SHRUBBY + (1|Geo_group), family = 'binomial', data=peatlands.filtered)
# m.rain = glmer(SevBin ~ scale(Mean_annual_rainfall) + (1|Geo_group), family = 'binomial', data=peatlands.filtered)
# m.elevation = glmer(SevBin ~ scale(Mean_elevation) + (1|Geo_group), family = 'binomial', data=peatlands.filtered)
# 
# AIC(m0, m.shrubby, m.rain, m.elevation) #Shrubby then rain is the best
# 
# m.global = glmer(SevBin ~ scale(AREA) + scale(PA_Ratio) + scale(PropTreeBuffer) + 
#                    scale(Mean_elevation) + scale(rain.anom.2019) + 
#                    scale(Mean_slope) + scale(ASPN) + scale(FF_max) + (1|Geo_group), family = 'binomial', data=peatlands.filtered)
# 
# DHARMa::testDispersion(m.global) # Looks fine
# summary(m.global)
# performance::r2(m.global) # Good R2
# 
# coefs = data.frame(summary(m.global)$coefficients)
# write.csv(coefs, "outputs/fsev_option2_coefs_new.csv")


# Plots
# 
# plot.model = m.global
# 
# area.pred= ggeffect(plot.model, terms = c("AREA"), ci.lvl = 0.95)
# area.plot= ggplot(area.pred) + 
#   geom_ribbon(aes(x=x, ymin = conf.low, ymax = conf.high), alpha = 0.4) + 
#   geom_line(aes(x=x, y = predicted)) + theme_cowplot() + 
#   theme_cowplot() + ylab("Probability Burnt Severely") + xlab("Peatland Area")
# 
# pa.pred= ggeffect(plot.model, terms = c("PA_Ratio"), ci.lvl = 0.95)
# pa.plot= ggplot(pa.pred) + 
#   geom_ribbon(aes(x=x, ymin = conf.low, ymax = conf.high), alpha = 0.4) + 
#   geom_line(aes(x=x, y = predicted)) + theme_cowplot() + 
#   theme_cowplot() + ylab("Probability Burnt Severely") + xlab("Perimeter-Area Ratio")
# 
# pt.pred= ggeffect(plot.model, terms = c("PropTreeBuffer"), ci.lvl = 0.95)
# pt.plot= ggplot(pt.pred) + 
#   geom_ribbon(aes(x=x, ymin = conf.low, ymax = conf.high), alpha = 0.4) + 
#   geom_line(aes(x=x, y = predicted)) + theme_cowplot() + 
#   theme_cowplot() + ylab("Probability Burnt Severely") + xlab("Surrounding Tree Cover")
# 
# elev.pred= ggeffect(plot.model, terms = c("Mean_elevation"), ci.lvl = 0.95)
# elev.plot= ggplot(elev.pred) + 
#   geom_ribbon(aes(x=x, ymin = conf.low, ymax = conf.high), alpha = 0.4) + 
#   geom_line(aes(x=x, y = predicted)) + theme_cowplot() + 
#   theme_cowplot() + ylab("Probability Burnt Severely") + xlab("Mean Elevation (m)")
# 
# slope.pred= ggeffect(plot.model, terms = c("Mean_slope"), ci.lvl = 0.95)
# slope.plot= ggplot(slope.pred) + 
#   geom_ribbon(aes(x=x, ymin = conf.low, ymax = conf.high), alpha = 0.4) + 
#   geom_line(aes(x=x, y = predicted)) + theme_cowplot() + 
#   theme_cowplot() + ylab("Probability Burnt Severely") + xlab("Mean Slope")
# 
# aspn.pred= ggeffect(plot.model, terms = c("ASPN"), ci.lvl = 0.95)
# aspn.plot= ggplot(aspn.pred) + 
#   geom_ribbon(aes(x=x, ymin = conf.low, ymax = conf.high), alpha = 0.4) + 
#   geom_line(aes(x=x, y = predicted)) + theme_cowplot() + 
#   theme_cowplot() + ylab("Probability Burnt Severely") + xlab("ASPN")
# 
# anom.pred= ggeffect(plot.model, terms = c("rain.anom.2019"), ci.lvl = 0.95)
# anom.plot= ggplot(anom.pred) + 
#   geom_ribbon(aes(x=x, ymin = conf.low, ymax = conf.high), alpha = 0.4) + 
#   geom_line(aes(x=x, y = predicted)) + theme_cowplot() + 
#   theme_cowplot() + ylab("Probability Burnt Severely") + xlab("Precipitation Percentile (2019)")
# 
# ff.pred = ggeffect(plot.model, terms = c("FF_max"), ci.lvl = 0.95)
# ff.plot= ggplot(ff.pred) + 
#   geom_pointrange(aes(x = x, y = predicted, ymin = conf.low, ymax = conf.high)) + 
#   theme_cowplot() + ylab("Probability Burnt Severely") + xlab("Times burnt since 1985")
# 
# 
# ## Tie together into one
# anysev_model_plot = cowplot::plot_grid(area.plot,
#                                        pa.plot,
#                                        pt.plot,
#                                        elev.plot,
#                                        slope.plot,
#                                        aspn.plot,
#                                        anom.plot,
#                                        ff.plot, nrow=2, labels = c("a)","b)","c)", "d)", "e)","f)", "g)", "h)"))
# 
# 
# ggsave(plot = anysev_model_plot, filename="anysevmodel_plot_new.pdf", path="outputs", device="pdf", 
#        width=6,height=4,units="in",scale=2.3)
