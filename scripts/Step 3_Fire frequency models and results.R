# Step 3 Analysis of fire frequency
library(lme4)
library(ggplot2)
library(ggeffects)
library(cowplot)
library(tidyverse)
freq.data = read.csv("data_clean/peatlands_frequency_data_new.csv")

names(freq.data)

# Check for correlations between covariates
df.cor = freq.data %>% 
  dplyr::select(HECTARES,AREA, Mean_elevation, Mean_slope, Mean_annual_min_temp,  rain.anom.2019,
                Mean_annual_max_temp, Mean_annual_rainfall, Aspect, ASPN, PA_Ratio, PropTreeBuffer) %>%
  cor()

corrplot::corrplot(df.cor)

# Plot some raw data to explore correlations with categorical variables
boxplot(freq.data$Mean_annual_rainfall ~ freq.data$SHRUBBY)
boxplot(freq.data$Mean_elevation ~ freq.data$SHRUBBY)
boxplot(freq.data$Mean_elevation ~ freq.data$ALPINE_EVC)
boxplot(freq.data$PropTree ~ freq.data$SHRUBBY)

boxplot(freq.data$Mean_elevation ~ freq.data$Geo_group)
boxplot(freq.data$Mean_annual_rainfall ~ freq.data$Geo_group)
boxplot(freq.data$AREA ~ freq.data$Geo_group)

##########################################
#### Number of times burnt since 1985 #### 
##########################################

### Questions and Models
# Plot variation across Geographic Areas
library(cowplot)

geo.labels = data.frame(Geo_group = unique(freq.data$Geo_group),
                        GeoLab = c("Baw Baw", "Buffalo", "Cobberas", "Bogong", "Snowy Range", "Lake Mountain"))

freq.data = left_join(freq.data, geo.labels)

freq.data.summary = freq.data %>% group_by(GeoLab) %>% summarise(y = mean(TimesBurnt), 
                                                                    ymin = quantile(TimesBurnt, prob=0.1), 
                                                                    ymax=quantile(TimesBurnt, prob=0.9)) 
pal = viridisLite::magma(n=7)[2:7]

geo_freq_plot = ggplot() + 
  geom_jitter(aes(x = freq.data$GeoLab, y=freq.data$TimesBurnt, colour = freq.data$GeoLab, alpha=0.4)) + 
  geom_pointrange(aes(x = freq.data.summary$GeoLab, 
                      y = freq.data.summary$y, 
                      ymin = freq.data.summary$ymin, 
                      ymax = freq.data.summary$ymax), size=1) +
  #geom_boxplot(aes(x=Geo_group, y = TimesBurnt, fill = Geo_group), alpha=0.4) + 
  scale_fill_manual(values=pal) + scale_color_manual(values=pal) + theme_cowplot() + 
  ylab("Number of times burnt since 1985") + xlab("") + theme(legend.position = "none")
geo_freq_plot

ggsave(plot = geo_freq_plot, filename="frequency_geogroup_new.pdf", path="outputs", device="pdf", 
       width=6,height=3,units="in",scale=1.25)

freq.data= filter(freq.data, Geo_group %in% c("Bogong Unit", "Buffalo", "Cobberas Unit", "Snowy Range"))

## Modelling as a Poisson Response Variable ##
# # Need to choose between Shrubby, Mean Annual Rainfall and Elevation due to correlation
# m0 = glm(TimesBurnt ~ Geo_group, family = 'poisson', data=freq.data)
# m0 = glmer(TimesBurnt ~ (1|Geo_group), family = 'poisson', data=freq.data)
# m.shrubby = glmer(TimesBurnt ~ SHRUBBY + (1|Geo_group), family = 'poisson', data=freq.data)
# m.rain = glmer(TimesBurnt ~ scale(Mean_annual_rainfall) + (1|Geo_group), family = 'poisson', data=freq.data)
# m.elevation = glmer(TimesBurnt ~ scale(Mean_elevation) + (1|Geo_group), family = 'poisson', data=freq.data)
# 
# AIC(m0, m.shrubby, m.rain, m.elevation) #Rainfall is the best
# 
# m.global = glmer(TimesBurnt ~ scale(AREA) + scale(PA_Ratio) + 
#                    scale(PropTreeBuffer) + scale(Mean_elevation) + 
#                    scale(Mean_slope) + scale(ASPN) + (1|Geo_group),
#                    family="poisson",
#                    data=freq.data)
# 
# DHARMa::testDispersion(m.global) # Model is underdispersed
# summary(m.global)
# performance::r2(m.global)
# 
# coefs = data.frame(summary(m.global)$coefficients)
# write.csv(coefs, "outputs/ff_option1_coefs_update_new.csv")

#### Binomial models burnt twice or more ####
m0 = glmer(MultiBurntBinary ~ (1|Geo_group), family = 'binomial', data=freq.data)
m.shrubby = glmer(MultiBurntBinary ~ SHRUBBY + (1|Geo_group), family = 'binomial', data=freq.data)
m.rain = glmer(MultiBurntBinary ~ scale(Mean_annual_rainfall) + (1|Geo_group), family = 'binomial', data=freq.data)
m.elevation = glmer(MultiBurntBinary ~ scale(Mean_elevation) + (1|Geo_group), family = 'binomial', data=freq.data)

AIC(m0, m.shrubby, m.rain, m.elevation) #Rainfall is the best

m.global = glmer(MultiBurntBinary ~ scale(AREA) + scale(PA_Ratio) + 
                   scale(PropTreeBuffer) + scale(Mean_annual_rainfall) + 
                   scale(Mean_slope) + scale(ASPN) + 
                   (1|Geo_group), family = 'binomial', data=freq.data)

DHARMa::testDispersion(m.global) # Looks fine

summary(m.global)
MuMIn::r.squaredGLMM(m.global)
performance::r2(m.global)

coefs = data.frame(summary(m.global)$coefficients)

write.csv(coefs, "outputs/ff_coefs_update_new.csv")

library(ggeffects)
library(cowplot)

# Area 
area.pred <- ggeffect(m.global, terms = c("AREA"), ci.lvl = 0.95)
area.plot = ggplot(area.pred) + 
  geom_ribbon(aes(x=x, ymin = conf.low, ymax = conf.high), alpha = 0.4) + 
  geom_line(aes(x=x, y = predicted)) + theme_cowplot() + ylim(0,1) +
  ylab("Pr(Burnt Twice or More)") + xlab("Peatland Area (m2)")

pa.pred <- ggeffect(m.global, terms = c("PA_Ratio"), ci.lvl = 0.95)
pa.plot = ggplot(pa.pred) + 
  geom_ribbon(aes(x=x, ymin = conf.low, ymax = conf.high), alpha = 0.4) + 
  geom_line(aes(x=x, y = predicted)) + theme_cowplot() + ylim(0,1) +
  ylab("Pr(Burnt Twice or More)") + xlab("Perimeter-area ratio")

tree.pred <- ggeffect(m.global, terms = c("PropTreeBuffer"), ci.lvl = 0.95)
tree.plot = ggplot(tree.pred) + 
  geom_ribbon(aes(x=x, ymin = conf.low, ymax = conf.high), alpha = 0.4) + 
  geom_line(aes(x=x, y = predicted)) + theme_cowplot() + ylim(0,1) +
  ylab("Pr(Burnt Twice or More)") + xlab("Surrounding Tree Cover")

# elev.pred <- ggpredict(m.global, terms = c("Mean_elevation [all]"), ci.lvl = 0.95)
# elev.plot = ggplot(elev.pred) + 
#   geom_ribbon(aes(x=x, ymin = conf.low, ymax = conf.high), alpha = 0.4) + 
#   geom_line(aes(x=x, y = predicted)) + theme_cowplot() + ylim(0,1) +
#   ylab("Pr(Burnt Twice or More)") + xlab("Mean Elevation (m)") 

rain.pred <- ggpredict(m.global, terms = c("Mean_annual_rainfall [all]"), ci.lvl = 0.95)
rain.plot = ggplot(rain.pred) + 
  geom_ribbon(aes(x=x, ymin = conf.low, ymax = conf.high), alpha = 0.4) + 
  geom_line(aes(x=x, y = predicted)) + theme_cowplot() + ylim(0,1) +
  ylab("Pr(Burnt Twice or More)") + xlab("Mean Annual Precipitation (mm)") 

## Slope
slope.pred = ggpredict(m.global, terms = c("Mean_slope [all]"),  ci.lvl = 0.95,  type = "re")
slope.plot = ggplot(slope.pred) + 
  geom_ribbon(aes(x=x, ymin = conf.low, ymax = conf.high), alpha = 0.4) +  
  geom_line(aes(x=x, y = predicted)) + theme_cowplot() + ylim(0,1) +
  ylab("Pr(Burnt Twice or More)") + xlab("Mean Slope") 

## Aspect
asp.pred = ggpredict(m.global, terms = c("ASPN [all]"), ci.lvl = 0.95)
asp.plot = ggplot(asp.pred) + 
  geom_ribbon(aes(x=x, ymin = conf.low, ymax = conf.high), alpha = 0.4) +  
  geom_line(aes(x=x, y = predicted)) + theme_cowplot() + ylim(0,1) +
  ylab("Pr(Burnt Twice or More)") + xlab("ASPN")
asp.plot

## Tie together into one
freq.model.plot = cowplot::plot_grid(area.plot, pa.plot, tree.plot,
                   rain.plot, slope.plot, asp.plot, nrow=2, 
                   labels = c("a)","b)","c)", "d)", "e)", "f)"))

freq.model.plot

ggsave(plot = freq.model.plot, filename="frequency_model_plot_new.pdf", path="outputs", device="pdf", 
       width=6,height=4,units="in",scale=2)
