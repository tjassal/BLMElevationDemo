#Title: AIMLoticEDA.R
# 25 Feb2024
#Author: Tim Assal
# objective: compare elevation (via PercentSlope) of AIM Lotic reaches; compare field elevation data with USGS 1 and 10 m 3DEP and elevatR data
# 3 parts to code: 1) prep Lotic AIM points for use in GEE; 2) run script in GEE; 3) import data from GEE and compare across data sets, 4) compare AIM data with elevatR data
# new user's should be able to start at step 3

# see elevationEDA.R for info on 1 m data availability

library(sf)
library(tidyverse)
library(gridExtra)
library(elevatr)

###################
# 1. Process AIM Lotic Data (needed for GEE)
##################

#load csvs
F.slopesummary<-read_csv("SourceData/AIM/F_SlopePoolSummary_W.csv")
aim.lotic<-read_csv("SourceData/AIM/I_Indicators.csv")
names(aim.lotic)

#check for NAs - note: plot on BullFrog creek has NA bottompairs; and a +longitude for top; this will catch it. 
aim.lotic<-aim.lotic %>% 
  drop_na(TopReachLatitude, TopReachLongitude, BottomReachLatitude, BottomReachLongitude, ProtocolReachLength, PctSlope) %>% 
  filter(TopReachLongitude<0|BottomReachLongitude<0) #ensure no longtitude's are positive

#create a .shp based on bottom and top of reach
TopReach<-st_as_sf(aim.lotic, coords = c("TopReachLongitude", "TopReachLatitude"), crs=4326, na.fail=FALSE) #set to WGS84
#note: I included na.fail b/c with default (=true), it fails due to missing values... but I don't see any missing values
BottomReach<-st_as_sf(aim.lotic, coords = c("BottomReachLongitude", "BottomReachLatitude"), crs=4326, na.fail=FALSE) #set to WGS84

plot(st_geometry(TopReach)) #ensure no rogue points
plot(st_geometry(BottomReach)) #ensure no rogue points

#subset attributes before export so I can keep things a little cleaner in GEE
TopReach<-TopReach %>% 
  select(EvaluationID, ProtocolReachLength, PctSlope)
BottomReach<-BottomReach %>% 
  select(EvaluationID, ProtocolReachLength, PctSlope)

#merge into one file
lotic.pairs<-rbind(TopReach, BottomReach)
plot(st_geometry(lotic.pairs))

#join with F.slopesummary to ensure only 'fully collected" slopes are included 
lotic.join<-lotic.pairs %>% full_join(F.slopesummary) #will join by EvaluationID by default
lotic.pairs.sub<-lotic.join %>% 
  filter(SlopeCollected=="Fully Collected") %>% 
  select(EvaluationID, ProtocolReachLength, PctSlope) %>%  #also subset columns for only what's needed
  drop_na #in case there are other attributes that are empty
lotic.pairs #sf object

#now export to run in GEE
st_write(lotic.pairs, "DerivedData/Lotic_AIM_sub.shp", driver = "ESRI Shapefile")

#note - this results in nearly 9,000 points
#GEE struggles if .shps have >5000

#subset into two data sets for ease in GEE
lotic.subA<-lotic.pairs.sub %>% 
  slice(1:4000)
st_write(lotic.subA, "DerivedData/Lotic_AIM_A.shp", driver = "ESRI Shapefile")

lotic.subB<-lotic.pairs.sub %>% 
  slice(4001:nrow(lotic.pairs.sub))
st_write(lotic.subB, "DerivedData/Lotic_AIM_B.shp", driver = "ESRI Shapefile")


###################
# 2. Google Earth Engine Proccessing 
##################
#See script GEE/ExtractElevationToPoint
#And I forgot - AIM data is in GEE!!!

###################
# 3. Compare Elevation from different sources
##################
#load GEE derived data back in
#note: there are two files for both the 1 m and 10 m (no significance; just merge them)

# 10 m
lotic.10mA<-read_csv("SourceData/GEE/lotic_elev10m-A.csv")
lotic.10mB<-read_csv("SourceData/GEE/lotic_elev10m-B.csv")
#merge 
lotic.10m<-rbind(lotic.10mA, lotic.10mB)
#clean up columns and rename to be consistent with original att names
names(lotic.10m)
lotic.10m<-lotic.10m %>% 
  select(-'system:index', -.geo) %>% 
  rename("EvaluationID"="EvltnID", "PctSlope"="PctSlop", "elevation10m"="elevation", "ProtocolReachLength"="PrtclRL")
#don't really need slope and reach length, but check for now to make sure they are correct
#prep data for reshape - convert from long to wide so my brain can fxn properly
lotic.10m.prep<-lotic.10m %>% 
  dplyr::group_by(EvaluationID) %>% 
  mutate(elev_id = row_number()) #this will ensure each elevation pair has a differnet identifier
names(lotic.10m.prep)
head(lotic.10m.prep)

#prep data for reshape data - convert from long to wide so my brain can fxn properly
lotic.10m.wide<-lotic.10m.prep %>% 
  pivot_wider(names_from = elev_id, values_from = c(elevation10m), names_sep="") %>% 
  rename("elev1"="1", "elev2"="2") %>% #rename the two elevation categories so they will be more intuitive
  mutate(elev.delta=(abs((elev1-elev2)))) %>% #calc elevation delta
  mutate(PctSlope.10m=(elev.delta/ProtocolReachLength)*100) #calc PctSlope from 10m elevation data
head(lotic.10m.wide)

cor.p10vField<-cor(x = lotic.10m.wide$PctSlope, y = lotic.10m.wide$PctSlope.10m, method = "pearson") #0.8141435
cor.p10vField


# 1 m
lotic.1mA<-read_csv("SourceData/GEE/lotic_elev1m-A.csv")
lotic.1mB<-read_csv("SourceData/GEE/lotic_elev1m-B.csv")
#merge 
lotic.1m<-rbind(lotic.1mA, lotic.1mB)
#clean up columns and rename to be consistent with original att names
names(lotic.1m)
lotic.1m<-lotic.1m %>% 
  select(-'system:index', -.geo) %>% 
  rename("EvaluationID"="EvltnID", "PctSlope"="PctSlop", "elevation1m"="elevation", "ProtocolReachLength"="PrtclRL")
#don't really need slope and reach length, but check for now to make sure they are correct
#prep data for reshape - convert from long to wide so my brain can fxn properly
lotic.1m.prep<-lotic.1m %>% 
  dplyr::group_by(EvaluationID) %>% 
  mutate(elev_id = row_number()) #this will ensure each elevation pair has a differnet identifier
names(lotic.1m.prep)
head(lotic.1m.prep)

#prep data for reshape data - convert from long to wide so my brain can fxn properly
lotic.1m.wide<-lotic.1m.prep %>% 
  pivot_wider(names_from = elev_id, values_from = c(elevation1m), names_sep="") %>% 
  rename("elev1"="1", "elev2"="2") %>% #rename the two elevation categories so they will be more intuitive
  mutate(elev.delta=(abs((elev1-elev2)))) %>% #calc elevation delta
  mutate(PctSlope.1m=(elev.delta/ProtocolReachLength)*100) #calc PctSlope from 10m elevation data
head(lotic.1m.wide)

cor.p1vField<-cor(x = lotic.1m.wide$PctSlope, y = lotic.1m.wide$PctSlope.1m, method = "pearson", use = "complete.obs") #0.819986
cor.p1vField


#join 1 m and 10 m data together to compare
join.lotic.check<-lotic.1m.wide %>% left_join(lotic.10m.wide, by="EvaluationID")

cor.p1v10<-cor(x = join.lotic.check$PctSlope.10m, y = join.lotic.check$PctSlope.1m, method = "pearson", use = "complete.obs") #0.8693022
cor.p1v10

#Plot
# scatterplot - 10 m vs. 1 m 
p1v10<-ggplot(join.lotic.check, aes(PctSlope.10m, PctSlope.1m)) + 
  geom_point(alpha=0.15)+ 
  xlim(0, 60)+ ylim(0, 60)+
  geom_abline(intercept = 0, slope = 1)+
  labs(title = "1 m USGS 3DEP vs. 10 m USGS 3DEP",
       subtitle = paste("Pearson Correlation=", round(cor.p1v10, 5)))+
  theme_bw()
p1v10
  

# scatterplot - 10 m vs. AIM field elevation
p10vField<-ggplot(join.lotic.check, aes(PctSlope.10m, PctSlope.x)) + 
  geom_point(alpha=0.15)+ 
  xlim(0, 60)+ ylim(0, 60)+
  geom_abline(intercept = 0, slope = 1)+
  labs(title = "10 m USGS 3DEP vs. AIM Field Data",
       subtitle = paste("Pearson Correlation=", round(cor.p10vField, 5)))+
  theme_bw()
p10vField

# scatterplot - 1 m vs. AIM field elevation
p1vField<-ggplot(join.lotic.check, aes(PctSlope.1m, PctSlope.x)) + 
  geom_point(alpha=0.15)+ 
  xlim(0, 60)+ ylim(0, 60)+
  geom_abline(intercept = 0, slope = 1)+
  labs(title = "1 m USGS 3DEP vs. AIM Field Data",
       subtitle = paste("Pearson Correlation=", round(cor.p1vField, 5)))+
  theme_bw()
p1vField
#compare plot
composite_map<-grid.arrange(p1v10, p10vField, p1vField, nrow=2)

###################
# 4. Compare USGS 3DEP Elevation Data with elevatr derived data 
##################
#note - join.lotic.check is a tibble; need to ensure it's an sf object and not a tibble
#need to convert foreign object (tibble) to sf using st_as_sf
?get_elev_point #src is either epgs or aws
#For point elevation data it uses USGS Elevation Point Query Service (United States only) - epqs-
#as well as the Amazon Web Services (AWS) Terrain Tiles from which point elevations are extracted

lotic.pairs.sub #AIM points
lotic.elevatr <- get_elev_point(lotic.pairs.sub, prj = 4326, src = "epqs") #fails b/c it's a tibble
#note: this did not fail on my PC; but it did on a mac; not sure why
#convert to sf
lotic.pairs.sub.sf<-st_as_sf(lotic.pairs.sub)
lotic.pairs.sub.sf #now sf object

lotic.elevatr.epqs <- get_elev_point(lotic.pairs.sub.sf, prj = 4326, src = "epqs") #cool, it works, takes a few mins
#note: there are holes in the 1m data availability...so there will. be a warning note of NAs generated (ignore)

lotic.elevatr.aws <- get_elev_point(lotic.pairs.sub.sf, prj = 4326, src = "aws") #cool, it works, takes a few secs, but it's coarse, results are bad
#An important thing to note, that the elevations will differ, and the prime reason is the resolution of 
#the AWS tiles at the specified zoom. The default zoom of 5 (i.e., z=5) is rather coarse and that is 
#reflected in the elevations.
#A larger zoom results in a smaller pixel size and the two sources converge.
lotic.elevatr.aws  <- get_elev_point(lotic.pairs.sub.sf, prj = 4326, src = "aws", z=12) #!!!!!fails b/c it's prompts not to download
#probably only good to use if a small area

#compare the epqs related elevtr data with USGS 3DEP data (don't bother with AWS)

#clean up columns and rename to be consistent with original att names
names(lotic.elevatr.epqs)
#convert from sf to tibble only to avoid error later on
lotic.elevatr.epqs<-as.tibble(lotic.elevatr.epqs) %>% 
  select(-geometry) 


#prep data for reshape - convert from long to wide so my brain can fxn properly
lotic.elevatr.epqs.prep<-lotic.elevatr.epqs %>% 
  dplyr::group_by(EvaluationID) %>% 
  mutate(elev_id = row_number()) #this will ensure each elevation pair has a different identifier
names(lotic.elevatr.epqs.prep)
head(lotic.elevatr.epqs.prep)

#prep data for reshape data - convert from long to wide so my brain can fxn properly
lotic.elevatr.epqs.wide<-lotic.elevatr.epqs.prep %>% 
  pivot_wider(names_from = elev_id, values_from = c(elevation), names_sep="") %>% 
  rename("elev1"="1", "elev2"="2") %>% #rename the two elevation categories so they will be more intuitive
  mutate(elev.delta=(abs((elev1-elev2)))) %>% #calc elevation delta
  mutate(PctSlope.epqs=(elev.delta/ProtocolReachLength)*100) #calc PctSlope from 10m elevation data
head(lotic.elevatr.epqs.wide)

cor.pElevtRvField<-cor(x = lotic.elevatr.epqs.wide$PctSlope, y=lotic.elevatr.epqs.wide$PctSlope.epqs, method = "pearson", use = "complete.obs") #0.8693022
cor.pElevtRvField

#Plot
# scatterplot - elevatr elevation vs. AIM Lotic field
pElevtRvField<-ggplot(lotic.elevatr.epqs.wide, aes(PctSlope, PctSlope.epqs)) + 
  geom_point(alpha=0.15)+ 
  xlim(0, 60)+ ylim(0, 60)+
  geom_abline(intercept = 0, slope = 1)+
  labs(title = "elevatR Data vs. AIM Field Data",
       subtitle = paste("Pearson Correlation=", round(cor.pElevtRvField, 5)))+
  theme_bw()
pElevtRvField


#compare plot - update
composite_map<-grid.arrange(p1v10, p10vField, p1vField,pElevtRvField, nrow=2)

# !!!!!!!
#!!!!!TAKE HOME - elevatR data correlates more strongly with AIM field elevation data than USGS 3DEP 1 or 10 m data
# !!!!!!

#export
ggsave("Graphics/AIMLotic-elevation-comparison.jpg", composite_map , device = "jpg")
