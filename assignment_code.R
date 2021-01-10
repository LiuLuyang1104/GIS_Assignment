
# Identification of “hot spots” of childhood obesity in London
## library packages
library(spatstat)
library(sp)
library(rgeos)
library(maptools)
library(GISTools)
library(tmap)
library(sf)
library(geojson)
library(geojsonio)
library(tmaptools)
library(dplyr)
library(readr)
library(ggplot2)
# loading data
LondonWards <- st_read('C:/Users/Louriee/Desktop/gis/assignment/Assignment/data/London-wards-2018/London-wards-2018_ESRI/London_Ward.shp')
LondonWardsMerged <- st_read("C:/Users/Louriee/Desktop/gis/assignment/Assignment/data/London-wards-2018/London-wards-2018_ESRI/London_Ward_CityMerged.shp")%>%
st_transform(.,27700)
library(readr)
obesity <- read_csv("C:/Users/Louriee/Desktop/gis/assignment/Assignment/data/obesity.csv")
# join data
LondonWardsObesity_whole <- LondonWardsMerged %>%
  left_join(obesity,
            by = c("GSS_CODE" = "new_code"))

# view map
tmap_mode("view")
tm_shape(LondonWardsObesity_whole) +tm_polygons(col = NA, alpha = 0.5)

# cleaning data
#drop the rows have NA values
LondonWardsObesity <- na.omit(LondonWardsObesity_whole)

# Part I descriptive analysis
#1. summary
summary(LondonWardsObesity$childhood_obesity)
summary(LondonWardsObesity$year6_obese_rate)
summary(LondonWardsObesity$reception_obese_rate)
#2. boxplot
x1<-LondonWardsObesity$childhood_obesity
x2<-LondonWardsObesity$reception_obese_rate
x3<-LondonWardsObesity$year6_obese_rate
df<-cbind("childhood"=x1,"reception year"=x2,"year 6"=x3)
boxplot(df)
#3. Histogram
Frequency_distribution<-ggplot(data=LondonWardsObesity,aes(x=childhood_obesity)) +
  geom_histogram(binwidth=5,fill="lightblue", breaks =c(0,0.03,0.06,0.09,0.12,0.15,0.18,0.21,0.24,0.27,0.3),
                 color="black", alpha=0.5)+
  theme_bw()+labs(x="Childhood Obesity Rate",y="Number of Wards",
                  title="Frequency distribution of childhood obesity rate in London wards")
Frequency_distribution + geom_vline(aes(xintercept=mean(LondonWardsObesity$childhood_obesity,
                                                        na.rm=TRUE)),
                                    color="red",
                                    linetype="dashed",
                                    size=1)+
  theme(plot.title = element_text(hjust = 0.5))
#compared by different age
compared_distribution<-ggplot(LondonWardsObesity) +
  geom_histogram(aes(x=LondonWardsObesity$reception_obese_rate,
                     color = "yellow",fill='Reception year obesity rate'),
                 color="black",bins=8, alpha=0.5) +
  geom_histogram(aes(x=LondonWardsObesity$year6_obese_rate,color = "pink",fill='Year 6 obesity rate'),
                 color="black",bins=8, alpha=0.5)+
  scale_fill_manual(values=c("yellow","pink"),'Data')+
  ggtitle("Histograms of 2 age groups childhood obesity rate") +
  xlab("Obesity Rate")+
  ylab("Numbers of Boroughs")+
  theme(  panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black")) +
  theme(plot.title = element_text(hjust = 0.5))
compared_distribution
#4. spatial distribution
tmap_mode("plot")
tm1 <- tm_shape(LondonWardsObesity_whole) +
  tm_polygons("reception_obese_rate",breaks=c(0,0.11,1),labels = c("obesity rate below mean", "obesity rate over mean"),title="Reception year obesity",
              palette="YlOrBr")+tm_legend(show=TRUE)+tm_layout(frame=FALSE)+tm_credits("(a)", position=c(0,0.85), size=1.5)+tm_compass(position=c("right", "top"))+tm_scale_bar()
tm2 <- tm_shape(LondonWardsObesity_whole) +
  tm_polygons("year6_obese_rate",breaks=c(0,0.23,1),labels = c("obesity rate below mean", "obesity rate over mean"),title="Year 6 obesity",
              palette="YlOrBr")+tm_legend(show=TRUE)+tm_layout(frame=FALSE)+tm_credits("(b)", position=c(0,0.85), size=1.5)+tm_compass(position=c("right", "top"))+tm_scale_bar()
t=tmap_arrange(tm1, tm2, ncol=2)
t
# PART II spatial auto correlation analysis
# step 1. define a spatial weights matrixweight
# calculate the centroids of all Wards in London
library(spdep)
coordsW <- LondonWardsObesity%>%
  st_centroid()%>%
  st_geometry()

plot(coordsW,axes=TRUE,ylim=c(155000, +202000),xlim=c(501000, +563000))

# create a neighbours list

neighbours <- LondonWardsObesity %>%
  poly2nb(., queen=T)

# plot neighbours
plot(neighbours, st_geometry(coordsW), col="green")
# add a map underneath
plot(LondonWardsObesity$geometry, add=T)

# create a spatial weights object from these weights
spatial_weights <- neighbours %>%
  nb2listw(., style="C")
head(spatial_weights$neighbours)
# step 2. calculate Global Moran's I and GetisOrdGi* statistic
# Global Moran's I
I_Obesity<- LondonWardsObesity %>%
pull(childhood_obesity) %>%
as.vector()%>%
moran.test(., spatial_weights)
I_Obesity

# GetisOrdGi*
G_Obesity <-LondonWardsObesity %>%
pull(childhood_obesity) %>%
as.vector()%>%
globalG.test(., spatial_weights)
G_Obesity

# step 3. calculate the local Moran's I and mapping
I_local_Obesity <- LondonWardsObesity %>%
pull(childhood_obesity) %>%
as.vector()%>%
localmoran(., spatial_weights)%>%
as_tibble()
slice_head(I_local_Obesity, n=5)
LondonWardsObesity <- LondonWardsObesity %>%mutate(Obesity_I = as.numeric(I_local_Obesity$Ii))
LondonWardsObesity <- LondonWardsObesity %>%mutate(Obesity_Iz =as.numeric(I_local_Obesity$Z.Ii))
breaks1<-c(-1000,-1.96,-1.65,1.65,1.96,1000)
MoranColours<- rev(brewer.pal(8, "RdGy"))
tm_shape(LondonWardsObesity) +tm_polygons("Obesity_Iz",tyle="fixed",
                                          breaks=breaks1,labels = c("low spatial auto correlation"," "," "," ", "high spatial auto correlation"),title="Childhood obesity rate",palette=MoranColours,midpoint=NA)+tm_legend(show=TRUE)+tm_layout(frame=FALSE,legend.position=c(0.78,0))+tm_credits("Local Moran'I of obesity rate", position=c(0,0.93), size=1.2)+tm_compass(position=c("right", "top"))+tm_scale_bar(position=c(0.05,0))
# step 4. calculate  the local Gi and mapping the hot spots and cold spots
Gi_local_Obesity <- LondonWardsObesity %>%
  pull(childhood_obesity) %>%
  as.vector()%>%
  localG(.,spatial_weights)

head(Gi_local_Obesity)
LondonWardsObesity <- LondonWardsObesity %>%
mutate(obesity_G = as.numeric(Gi_local_Obesity))
GIColours<- rev(brewer.pal(8, "RdBu"))

#plot local gi map
tm_shape(LondonWardsObesity) +tm_polygons("obesity_G",tyle="fixed",
                                           breaks=breaks1,labels = c("95% cold spots","90% cold spots","not significant","90% hot spots","95% hot spots"),title="Childhood obesity rate",palette=GIColours,midpoint=NA)+tm_legend(show=TRUE)+tm_layout(frame=FALSE,legend.position=c(0.78,0))+tm_credits("Gi* of obesity rate", position=c(0,0.93), size=1.2)+tm_compass(position=c("right", "top"))+tm_scale_bar(position=c(0.05,0))
#compare the distribution of hot spots of different age groups.
# reception year
# local Moran's I
I_local_reception_Obesity <- LondonWardsObesity %>%
  pull(reception_obese_rate) %>%
  as.vector()%>%
  localmoran(., spatial_weights)%>%
  as_tibble()
slice_head(I_local_reception_Obesity, n=5)
LondonWardsObesity <- LondonWardsObesity %>%mutate(reception_Obesity_I = as.numeric(I_local_reception_Obesity$Ii))
LondonWardsObesity <- LondonWardsObesity %>%mutate(reception_Obesity_Iz =as.numeric(I_local_reception_Obesity$Z.Ii))
# local Gi
Gi_local_reception_Obesity <- LondonWardsObesity %>%
  pull(reception_obese_rate) %>%
  as.vector()%>%
  localG(.,spatial_weights)

head(Gi_local_reception_Obesity)
LondonWardsObesity <- LondonWardsObesity %>%
  mutate( reception_Obesity_G= as.numeric(Gi_local_reception_Obesity))
GIColours<- rev(brewer.pal(8, "RdBu"))

# year 6
# local Moran's I
I_local_year6_Obesity <- LondonWardsObesity %>%
  pull(year6_obese_rate) %>%
  as.vector()%>%
  localmoran(., spatial_weights)%>%
  as_tibble()
slice_head(I_local_year6_Obesity, n=5)
LondonWardsObesity <- LondonWardsObesity %>%mutate(year6_Obesity_I = as.numeric(I_local_year6_Obesity$Ii))
LondonWardsObesity <- LondonWardsObesity %>%mutate(year6_Obesity_Iz =as.numeric(I_local_year6_Obesity$Z.Ii))

# local Gi
Gi_local_year6_Obesity <- LondonWardsObesity %>%
  pull(year6_obese_rate) %>%
  as.vector()%>%
  localG(.,spatial_weights)

head(Gi_local_year6_Obesity)
LondonWardsObesity <- LondonWardsObesity %>%
  mutate( year6_Obesity_G= as.numeric(Gi_local_year6_Obesity))
GIColours<- rev(brewer.pal(8, "RdBu"))

#plot the reception year and year 6's local Gi map
tmap_mode("plot")
tm_reception <- tm_shape(LondonWardsObesity) +
  tm_polygons("reception_Obesity_G",breaks=breaks1,title="Reception year obesity",
              palette=GIColours,midpoint=NA)+tm_legend(show=FALSE)+tm_layout(frame=FALSE)+tm_credits("(a) Gi* of Reception year's children obesity rate", position=c(0.05,0.85),size=1.3)+tm_scale_bar(position=c(0.1,0))
tm_year6 <- tm_shape(LondonWardsObesity) +
  tm_polygons("year6_Obesity_G",breaks=breaks1,labels = c("95% cold spots","90% cold spots","not significant","90% hot spots","95% hot spots"),title=" ",
              palette=GIColours,midpoint=NA)+tm_legend(show=TRUE)+tm_layout(frame=FALSE)+tm_credits("(b) Gi* of year 6's children obesity rate", position=c(0,0.85), size=1.2)+tm_compass(position=c("right", "top"),size=1.5)
t2=tmap_arrange(tm_reception, tm_year6, ncol=2)
t2

