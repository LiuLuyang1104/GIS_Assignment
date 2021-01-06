#first library a few packages that we will use during the practical
#note you may need to install them first...
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

# loading data
LondonWards <- st_read('C:/Users/Louriee/Desktop/gis/assignment/Assignment/London-wards-2018/London-wards-2018_ESRI/London_Ward.shp')
LondonWardsMerged <- st_read("C:/Users/Louriee/Desktop/gis/assignment/Assignment/London-wards-2018/London-wards-2018_ESRI/London_Ward_CityMerged.shp")%>%
st_transform(.,27700)
library(readr)
obesity <- read_csv("obesity.csv")
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
#drop the symbol"%"and change to
Childhood_Obesity<-data.frame(lapply(obesity$childhood_obesity,
                                     function(x) as.numeric(sub("%", "", x))/100) )
Reception_Obese_rate<-data.frame(lapply(obesity$reception_obese_rate,
                                        function(x) as.numeric(sub("%", "", x))/100) )
Year6_Obese_rate<-data.frame(lapply(obesity$year6_obese_rate,
                                    function(x) as.numeric(sub("%", "", x))/100) )
obesity<-obesity%>%mutate(t(Childhood_Obesity),t(Reception_Obese_rate),
                          t(Year6_Obese_rate))
#descriptive analysis
#1. general analysis
#mean,median,max,min
#frequency distribution
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
#spatial distribution
tm_shape(LondonWardsObesity_whole) +
  tm_polygons("reception_obese_rate",
              style="fixed",
              breaks=c(0.05,0.1,0.15,0.2),
              palette=GIColours,
              midpoint=NA,
              title="Gi*, Obesity in London")
# define a spatial weights matrixweight
#First calculate the centroids of all Wards in London
library(spdep)
coordsW <- LondonWardsObesity%>%
  st_centroid()%>%
  st_geometry()

plot(coordsW,axes=TRUE,ylim=c(155000, +202000),xlim=c(501000, +563000))

#create a neighbours list

neighbours <- LondonWardsObesity %>%
  poly2nb(., queen=T)

#plot them
plot(neighbours, st_geometry(coordsW), col="green")
#add a map underneath
plot(LondonWardsObesity$geometry, add=T)

#create a spatial weights object from these weights
spatial_weights <- neighbours %>%
  nb2listw(., style="C")
head(spatial_weights$neighbours)
# spatial correlation analysis
# Moran's I
I_Obesity<- LondonWardsObesity %>%
pull(childhood_obesity) %>%
as.vector()%>%
moran.test(., spatial_weights)
I_Obesity
#c
C_Obesity <- LondonWardsObesity %>%
pull(childhood_obesity) %>%
as.vector()%>%
geary.test(., spatial_weights)
C_Obesity
#
G_Obesity <-LondonWardsObesity %>%
pull(childhood_obesity) %>%
as.vector()%>%
globalG.test(., spatial_weights)
G_Obesity

#local Moran's
I_local_Obesity <- LondonWardsObesity %>%
pull(childhood_obesity) %>%
as.vector()%>%
localmoran(., spatial_weights)%>%
as_tibble()
slice_head(I_local_Obesity, n=5)
LondonWardsObesity <- LondonWardsObesity %>%mutate(Obesity_I = as.numeric(I_local_Obesity$Ii))
LondonWardsObesity <- LondonWardsObesity %>%mutate(Obesity_Iz =as.numeric(I_local_Obesity$Z.Ii))
breaks1<-c(-1000,-2.58,-1.96,-1.65,1.65,1.96,2.58,1000)
MoranColours<- rev(brewer.pal(8, "RdGy"))
tm_shape(LondonWardsObesity) + tm_polygons("Obesity_Iz",tyle="fixed",
                    breaks=breaks1,
                    palette=MoranColours,
                    midpoint=NA,
                    title="Local Moran's I, Obesity in London")
#local Gi
Gi_local_Obesity <- LondonWardsObesity %>%
  pull(childhood_obesity) %>%
  as.vector()%>%
  localG(.,spatial_weights)

head(Gi_local_Obesity)
LondonWardsObesity <- LondonWardsObesity %>%
mutate(density_G = as.numeric(Gi_local_Obesity))
GIColours<- rev(brewer.pal(8, "RdBu"))

#now plot on an interactive map
tm_shape(LondonWardsObesity) +
  tm_polygons("density_G",
              style="fixed",
              breaks=breaks1,
              palette=GIColours,
              midpoint=NA,
              title="Gi*, Obesity in London")
