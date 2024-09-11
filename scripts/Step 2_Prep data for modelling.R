
# Step 2 Clean Nick's data up to create modelling dataset 
library(dplyr)
library(tidyr)
library(sf)
library(raster)
library(fasterize)
library(exactextractr)

data.path = "~/Library/CloudStorage/OneDrive-TheUniversityofMelbourne/Supervision/2022-23/Nick_AlpineBogs/AlpineBogsAnalysis/"

peatlands.data = readxl::read_xlsx(paste0(data.path, "data/PEATLANDS_FIRE_HISTORY_INFLUENCES_update.xlsx"), sheet="Peatlands_FF_FS")
peatlands.rain = readxl::read_xlsx(paste0(data.path,"data/PEATLANDS_FIRE_HISTORY_INFLUENCES_update.xlsx"), sheet="Rainfall_Temp")
peatlands.metrics = readxl::read_xlsx(paste0(data.path,"data/PEATLANDS_FIRE_HISTORY_INFLUENCES_update.xlsx"), sheet="Metrics")
peatlands.aspect = readxl::read_xlsx(paste0(data.path,"data/PEATLANDS_FIRE_HISTORY_INFLUENCES_update.xlsx"), sheet="Aspect")

#######################################
#### Extract additional covariates ####
#######################################

####################
#### TREE COVER ####
####################

## Tree Cover within Peatlands ## 
tree = read_sf("data/TREE100.shp") %>% mutate(Tree = 1) %>% st_transform(3111)
r = raster(ext= extent(tree), crs = crs(tree), res = 30)
tree_r = fasterize(tree, r, field="Tree")
peatlands = read_sf("data/For_Billy.shp") %>% st_transform(st_crs(tree)) %>% group_by(FID_bog, AREA) %>% summarise()

peatlands$AreaTree = (exact_extract(tree_r, peatlands, fun = "sum") * res(tree_r)^2)
peatlands$PropTree = peatlands$AreaTree / peatlands$AREA
peatlands$PropTree = ifelse(peatlands$PropTree >1, 1, peatlands$PropTree)
peatlands.trees = peatlands %>% dplyr::select(FID_bog,AREA, AreaTree, PropTree) %>% st_drop_geometry()

## Tree cover in 500m surrounding peatlands ## 
peatlands_buffer = peatlands %>% st_buffer(dist=500) 
peatlands.trees$AreaBuffer = as.numeric(st_area(peatlands_buffer))
peatlands.trees$AreaTreeBuffer = (exact_extract(tree_r, peatlands_buffer, fun = "sum") * res(tree_r)^2)

peatlands.trees$AreaBufferOnly = peatlands.trees$AreaBuffer - peatlands.trees$AREA
peatlands.trees$AreaTreeBufferOnly = peatlands.trees$AreaTreeBuffer - peatlands.trees$AreaTree

peatlands.trees$PropTreeBuffer = peatlands.trees$AreaTreeBufferOnly / peatlands.trees$AreaBufferOnly
peatlands.trees$PropTreeBuffer = ifelse(peatlands.trees$PropTreeBuffer >1, 1, peatlands.trees$PropTreeBuffer)

peatlands.trees = peatlands.trees %>% dplyr::select(FID_bog, PropTree, PropTreeBuffer)

###################################
#### ADD RAINFALL ANOMALY DATA ####
###################################
rain.anom = raster("data/precip_percentile_r005_20190101_20191231.nc")
peatlands = peatlands %>% st_transform(crs(rain.anom))
peatlands.rain.anom = data.frame(FID_bog = as.factor(peatlands$FID_bog), rain.anom.2019 = exact_extract(rain.anom, peatlands, fun="mean"))


###########################################
#### Fire Frequency Data for Modelling ####
###########################################

# Widen
peatlands.ff = peatlands.data %>%
  group_by(FID_bog,Geo_group, PLANT_COMM, SOURCE, AREA, PERIMETER, HECTARES, ALPINE_EVC, SHRUBBY, FF_Since_1985) %>%
  summarise(FF_Area = sum(FF_FS_Area))

peatlands.timesburnt = peatlands.ff %>% group_by(FID_bog) %>% summarise(TimesBurnt = max(FF_Since_1985))

peatlands.ff.wide = peatlands.ff %>% 
  pivot_wider(id_cols = c(FID_bog,Geo_group, PLANT_COMM, SOURCE, AREA, PERIMETER, HECTARES, ALPINE_EVC, SHRUBBY), 
              names_from = FF_Since_1985, values_from = FF_Area, names_prefix = "AreaBurnt_", values_fill=0) %>%
  mutate(AreaMultiBurnt = AreaBurnt_2 + AreaBurnt_3 + AreaBurnt_4 + AreaBurnt_5) %>%
  mutate(PropMultiBurnt = AreaMultiBurnt/(AreaBurnt_0 + AreaBurnt_1 + AreaBurnt_2 + AreaBurnt_3 + AreaBurnt_4 + AreaBurnt_5)) %>% 
  mutate(MultiBurntBinary = ifelse(PropMultiBurnt >0,1,0)) %>% 
  ungroup()

peatlands.aspect.mode = peatlands.aspect %>% 
  group_by(FID_bog...1) %>%
  filter(Shape_Area == max(Shape_Area)) %>%
  arrange(FID_bog...1, Shape_Area, gridcode) %>%
  dplyr::select(FID_bog...1, Shape_Area, "Aspect"=gridcode)

peatlands.aspect.mode$Aspect = ifelse(peatlands.aspect.mode$Aspect == -1, 0, peatlands.aspect.mode$Aspect)
peatlands.aspect.mode$ASPN = ifelse(peatlands.aspect.mode$Aspect > 180, abs(peatlands.aspect.mode$Aspect-360), peatlands.aspect.mode$Aspect)

# Compile into one data frame for modelling
peatlands.ff.wide = left_join(peatlands.ff.wide, peatlands.timesburnt, by="FID_bog")
peatlands.ff.wide = left_join(peatlands.ff.wide, peatlands.metrics, by="FID_bog")
peatlands.ff.wide = left_join(peatlands.ff.wide, peatlands.rain, by="FID_bog")
peatlands.ff.wide = left_join(peatlands.ff.wide, peatlands.aspect.mode, by=c("FID_bog"="FID_bog...1"))
peatlands.ff.wide$Exposure = as.factor(peatlands.ff.wide$`Exposure (5 = Lower; 6 = Higher)`)
peatlands.ff.wide$Geo_group = as.factor(peatlands.ff.wide$Geo_group)
peatlands.ff.wide$PA_Ratio = peatlands.ff.wide$PERIMETER / peatlands.ff.wide$AREA

peatlands.ff.wide = left_join(peatlands.ff.wide, peatlands.trees, by="FID_bog")

# Add anomaly data
peatlands.ff.wide$FID_bog <- as.factor(peatlands.ff.wide$FID_bog)

peatlands.ff.wide = left_join(peatlands.ff.wide, peatlands.rain.anom, by="FID_bog")

# Save
write.csv(peatlands.ff.wide, "data_clean/peatlands_frequency_data_new.csv")

##########################################
#### Fire Severity Data for Modelling ####
##########################################

peatlands.sev.ff = peatlands.data %>%
  group_by(FID_bog,Geo_group, PLANT_COMM, SOURCE, AREA, PERIMETER, HECTARES, ALPINE_EVC, SHRUBBY, FF_Since_1985, `FS_2019/2020`) %>%
  summarise(FF_FS_Area = sum(FF_FS_Area))

peatlands.sev.area = peatlands.sev.ff %>% 
  filter(`FS_2019/2020` %in% c(4,5,6)) %>% 
  group_by(FID_bog) %>% 
  summarise(Area_Severe = sum(FF_FS_Area))

peatlands.sev.ff = peatlands.sev.ff %>% 
  group_by(FID_bog,Geo_group, PLANT_COMM, SOURCE, AREA, PERIMETER, HECTARES, ALPINE_EVC, SHRUBBY) %>%
  summarise(FF_max = max(FF_Since_1985), FS_max = max(`FS_2019/2020`))

peatlands.sev.ff = left_join(peatlands.sev.ff, peatlands.sev.area, by="FID_bog")
peatlands.sev.ff$Area_Severe[is.na(peatlands.sev.ff$Area_Severe)] <-0
peatlands.sev.ff$PropSeverelyBurnt = peatlands.sev.ff$Area_Severe/peatlands.sev.ff$AREA
peatlands.sev.ff$PropSeverelyBurnt = ifelse(peatlands.sev.ff$PropSeverelyBurnt>1,1, peatlands.sev.ff$PropSeverelyBurnt)

peatlands.aspect.mode = peatlands.aspect %>% 
  group_by(FID_bog...1) %>%
  filter(Shape_Area == max(Shape_Area)) %>%
  arrange(FID_bog...1, Shape_Area, gridcode) %>%
  dplyr::select(FID_bog...1, Shape_Area, "Aspect"=gridcode)

peatlands.aspect.mode$Aspect = ifelse(peatlands.aspect.mode$Aspect == -1, 0, peatlands.aspect.mode$Aspect)
peatlands.aspect.mode$ASPN = ifelse(peatlands.aspect.mode$Aspect > 180, abs(peatlands.aspect.mode$Aspect-360), peatlands.aspect.mode$Aspect)

# Compile into one data frame for modelling
peatlands.sev.ff = left_join(peatlands.sev.ff, peatlands.timesburnt, by="FID_bog")
peatlands.sev.ff = left_join(peatlands.sev.ff, peatlands.metrics, by="FID_bog")
peatlands.sev.ff = left_join(peatlands.sev.ff, peatlands.rain, by="FID_bog")
peatlands.sev.ff = left_join(peatlands.sev.ff, peatlands.aspect.mode, by=c("FID_bog"="FID_bog...1"))
peatlands.sev.ff$Exposure = as.factor(peatlands.sev.ff$`Exposure (5 = Lower; 6 = Higher)`)
peatlands.sev.ff = left_join(peatlands.sev.ff, peatlands.trees, by="FID_bog")

peatlands.sev.ff$PA_Ratio = peatlands.sev.ff$PERIMETER / peatlands.sev.ff$AREA

# Create fire severity categories based on ground truthing
peatlands.sev.ff$MaxSevCat = ifelse(peatlands.sev.ff$FS_max > 3, "High", 
                                    ifelse(peatlands.sev.ff$FS_max < 2, "Unburnt", "Low"))

## This is creating a binary variable where if there is at least some part of a peatland that has a max fire severity > 3, then it gets a 1
peatlands.sev.ff$SevBin = ifelse(peatlands.sev.ff$FS_max > 3, 1, 0)

# Additional Binary variable based on coverage of peatland by severe fire 
peatlands.sev.ff$SevCov = ifelse(peatlands.sev.ff$PropSeverelyBurnt >0.5, 1, 0)

#write.csv(peatlands.sev.ff, "data/peatlands_sevpeatlands")

# Add rainfall anomaly data
peatlands.sev.ff$FID_bog <- as.factor(peatlands.sev.ff$FID_bog)

peatlands.sev.ff = left_join(peatlands.sev.ff, peatlands.rain.anom, by="FID_bog")

## Save
write.csv(peatlands.sev.ff, "data_clean/peatlands_severity_data_new.csv")

