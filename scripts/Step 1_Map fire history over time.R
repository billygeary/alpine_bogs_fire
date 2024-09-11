
# Step One - Fire history over time

library(sf)
library(dplyr)
library(tidyr)
library(raster)
library(ggplot2)
library(cowplot)

fh = read_sf("~/Dropbox/Billy/_research/_DELWP/Double Triple Burns/FIRE_HISTORY.shp")
peatlands = read_sf("data/For_Billy.shp")
peatlands$Dummy = 1

# Create base
mask = raster(crs=crs(peatlands), ext = extent(peatlands), resolution = 25)
values(mask) <- 0

#Peatland Raster
peatlands.ras = fasterize::fasterize(peatlands, mask, field = "Dummy")

# Loop through fire years
fire_in = fh %>%
  filter(FIRETYPE == "BUSHFIRE") %>%
  st_transform(crs = crs(mask))

years = sort(unique(fire_in$SEASON))
years = years[years > 1939]
fire_culm = mask
fire.years = list()
fire.culmulative = list()
peatlands.burnt = data.frame()
for (y in 1:length(years)){
  year = years[y]
  fire_subset = filter(fire_in, SEASON == year)
  fire_subset_ras = fasterize::fasterize(fire_subset, mask, fun='last', background=0)
  fire_culm = fire_culm + fire_subset_ras
  
  names(fire_subset_ras) <- year
  names(fire_culm) <- year
  
  out = zonal(peatlands.ras, fire_culm, fun='sum')
  out = data.frame(out)
  out$Year = year
  out$HDMSumsHa = (out$sum*25^2)/10000
  peatlands.burnt = rbind(peatlands.burnt, out)
}

# head(peatlands.burnt)

# Plot results and save
plot = peatlands.burnt %>% dplyr::filter(!zone %in% c(0,7)) %>%
  ggplot() +
  geom_area(aes(x=Year, y=HDMSumsHa, fill = as.factor(zone))) + 
  scale_fill_viridis_d(name="Times Burned", option="D") +
  ylab("Cumulative area of peatlands burnt at least once (Ha)") + theme_cowplot()
plot

pal = RColorBrewer::brewer.pal(7, "Oranges")
pal = pal[2:7]

plot = peatlands.burnt %>% dplyr::filter(!zone %in% c(0,7)) %>%
  ggplot() +
  geom_area(aes(x=Year, y=HDMSumsHa/4381, fill = as.factor(zone))) + 
  geom_vline(xintercept = 2003, linetype='dashed') +
  geom_vline(xintercept = 2007, linetype='dashed') +
  geom_vline(xintercept = 2020, linetype='dashed') +
  #scale_fill_viridis_d(name="Times Burned", option="D") + 
  scale_fill_manual(values = pal, name = "Times Burned") +
  ylim(0, 1) + xlim(1940, 2023) +
  ylab("Cumulative proportion of \n peatland area burnt at least once") + theme_cowplot()
plot

ggsave(plot = plot, filename="cumulative_peatland_plot.pdf", path="outputs", device="pdf", 
       width=6,height=4,units="in",scale=2)


# 19/20 Fire Severity
sev = raster("data/FireSeverityFinal_20200414.tif")

peatlands.burnt = exactextractr::exact_extract(sev, peatlands, function(value, area) sum(area[value > 2], na.rm=TRUE), coverage_area = TRUE, stack_apply=TRUE)
peatlands.sev = exactextractr::exact_extract(sev, peatlands, function(value, area) sum(area[value > 4], na.rm=TRUE), coverage_area = TRUE, stack_apply=TRUE)

propsev = peatlands.sev/peatlands$AREA

peatlands.data = data.frame(peatlands.burnt, peatlands.sev, propsev)

peatlands.data.burnt = filter(peatlands.data, peatlands.burnt>0)

hist(peatlands.data.burnt$propsev)
