# basic statistics for the manuscript
#Load data####
pheno_early <- read.csv('data/Apfel_Bluehdaten_fruehe_Reifezeit.csv') %>% 
  filter(Referenzjahr >= 1993)
pheno_late <- read.csv('data/Apfel_Bluehdaten_spaete_Reifezeit.csv') %>% 
  filter(Referenzjahr >= 1993)

pheno_early<-subset(pheno_early, pheno_early$Phase_id==5 & pheno_early$Referenzjahr<2023)
pheno_late<-subset(pheno_late, pheno_late$Phase_id==5& pheno_late$Referenzjahr<2023)

#number of observations####
n_early<-length(pheno_early$Objekt_id)
n_late<-length(pheno_late$Objekt_id)

n_all<-n_early+n_late

n_early
n_late
n_all


#GCMs per scenario
change_scenarios<-read.csv("data/change_scenarios.csv")
ssp126_change<-subset(change_scenarios, change_scenarios$scenario=="ssp126")
unique(ssp126_change$labels)
unique(change_scenarios$labels)

change_2085<-subset(change_scenarios, change_scenarios$scenario_year==2085)
unique(change_2085$labels)

ssp245_change<-subset(change_scenarios, change_scenarios$scenario=="ssp245")
unique(ssp245_change$labels)

ssp370_change<-subset(change_scenarios, change_scenarios$scenario=="ssp370")
unique(ssp370_change$labels)

ssp585_change<-subset(change_scenarios, change_scenarios$scenario=="ssp585")
unique(ssp585_change$labels)


#bloomdate shift ####
future_damage_files<-c(list.files("frostdamage/data/output"))

bloomdate_example_list <- list()
for (i in 1:length(future_damage_files)){
  frost_damage_file<-readRDS(paste0("frostdamage/data/output/",future_damage_files[i]))
  frost_damage_file$pathway<-sub(".*\\.(ssp\\d+).*", "\\1", frost_damage_file$id)
  frost_damage_file$model<-sub(".*\\.ssp\\d+\\.(\\w+)\\.\\d+$", "\\1", frost_damage_file$id)
  frost_damage_file$simyear<-sub(".*\\.(\\d+)$", "\\1", frost_damage_file$id)
  frost_damage_file$Station_id<-sub(".*\\.(\\d+)\\.ssp.*", "\\1", frost_damage_file$id)
  frost_damage_file$repetition<-sub("^[^.]+\\.(\\d+)\\..*", "\\1", frost_damage_file$id)
  frost_damage_file$ripening<-sub("^([^.]+)\\..*", "\\1", frost_damage_file$id)

  bloomdate_example_list[[i]] <- frost_damage_file
}

bloomdate_example_df <- bind_rows(bloomdate_example_list)
bloomdate_relevant<-bloomdate_example_df[c(4,5,8,9,10,11,13)]

bloomdate<-pheno_early$Jultag
season<-pheno_early$Referenzjahr
pathway<-rep("hist", length(pheno_early$Jultag))
model<-rep("hist", length(pheno_early$Jultag))
simyear<-rep("1993-2022", length(pheno_early$Jultag))
Station_id<-pheno_early$Stations_id
ripening<-rep("early", length(pheno_early$Jultag))

bloomdate_early<-data.frame(bloomdate,season,pathway, model, simyear, Station_id, ripening)

bloomdate<-pheno_late$Jultag
season<-pheno_late$Referenzjahr
pathway<-rep("hist", length(pheno_late$Jultag))
model<-rep("hist", length(pheno_late$Jultag))
simyear<-rep("1993-2022", length(pheno_late$Jultag))
Station_id<-pheno_late$Stations_id
ripening<-rep("late", length(pheno_late$Jultag))

bloomdate_late<-data.frame(bloomdate,season,pathway, model, simyear, Station_id, ripening)

bloomdata_data_new_plot<-rbind(bloomdate_relevant, bloomdate_early, bloomdate_late)

##read relevant bloomdate####
#saveRDS(bloomdata_data_new_plot, "data/bloomdate_overview_plot.rds")
bloomdata_data_new_plot<-readRDS( "data/bloomdate_overview_plot.rds")

stderror <- function(x) sd(x)/sqrt(length(x))

bloomdate_data_new_plot_summary<-bloomdata_data_new_plot%>%
  group_by(pathway,model,simyear,ripening)%>%
  summarise(mean=mean(bloomdate), sd=sd(bloomdate), median=median(bloomdate), SE= stderror(bloomdate), q45=quantile(bloomdate, 0.45), q55=quantile(bloomdate, 0.55))

bloomdate_data_new_plot_summary$width2<-c(0.08,0.08,rep(1.0,length(bloomdate_data_new_plot_summary$model)-2))

early_late_facet_labels <- c(
  `early` = "early-ripening apple varieties",
  `late` = "late-ripening apple varieties"
)


library(stringr)
bloomdate_data_new_plot_summary$pathway <- str_replace(bloomdate_data_new_plot_summary$pathway, "hist", "historical")
bloomdate_data_new_plot_summary$model <- str_replace(bloomdate_data_new_plot_summary$model, "hist", "historical")


bloomdate_data_new_plot_summary <- bloomdate_data_new_plot_summary %>%
  mutate(model = recode(model, "median" = "average", "quantile10" = "optimistic","quantile90"="pessimistic"))
bloomdate_data_new_plot_summary <- bloomdate_data_new_plot_summary %>%
  mutate(pathway = recode(pathway, "ssp126" = "SSP1-2.6", "ssp245" = "SSP2-4.5","ssp370"="SSP3-7.0", "ssp585"="SSP5-8.5"))
bloomdate_data_new_plot_summary$model_2<-factor(bloomdate_data_new_plot_summary$model, levels=c("historical", "optimistic" , "average","pessimistic"))


png("figures/new_names_bloomdate_future.png", pointsize=8,  width=4560, height=3200, res=600)
ggplot(bloomdate_data_new_plot_summary, aes(x = simyear, y = median, col=pathway, shape= model_2, linetype=model_2)) +
  geom_point(position=position_dodge(width=0.7), size=3) +
  #geom_jitter(width=0.5)+
  geom_errorbar(data=bloomdate_data_new_plot_summary,
                aes(ymin = q45, 
                    ymax = q55,
                    width = width2),
                position=position_dodge(width=0.7))+
  facet_grid(~ripening,labeller = as_labeller(early_late_facet_labels))+
  theme_bw()+
  xlab("Year")+
  ylab("Start of apple bloom")+
  scale_shape_manual(values = c(18,17,16,4), name = "summarized\nGCMs")+
  scale_linetype_manual(values = c("solid", "dotdash", "solid", "dotdash"), name = "summarized\nGCMs")+
  scale_y_continuous(breaks = c(100, 105,110,115,120), labels = c(expression("April 10"^"th"),
                                                                  expression("April 15"^"th"),
                                                                  expression("April 20"^"th"),
                                                                  expression("April 25"^"th"),
                                                                  expression("April 30"^"th")))+
  theme(axis.text = element_text(colour = "black"))+
  scale_color_discrete(name="pathway")
dev.off()



#Map station locations####

map_data_all<-read.csv("data/frost_damage_summary_new.csv")
map_data_early<-subset(map_data_all, map_data_all$ripening=="early")

all_pheno_stations<-read.csv("data/all_pheno_stations.csv")
new_names<-c(location="Stationsname", Station_id="Stations_id")
all_pheno_stations<-rename(all_pheno_stations, all_of(new_names) )


map_data_early<-merge(map_data_early, all_pheno_stations[2:6], by="Station_id")
map_data_red<-subset(map_data_early, map_data_early$pathway=="ssp126" &map_data_early$model=="median"& map_data_early$simyear=="2050")

library(sf) #'simple features' package
library(leaflet) # web-embeddable interactive maps
library(ggplot2) # general purpose plotting
library(rnaturalearth) # map data
library(rnaturalearthdata)# map data
library(ggspatial) # scale bars and north arrows
library(rnaturalearthhires)# map data 

wetterstationen<-read.csv("data/Wetter_1992_2022.csv")

wetterstationen_1<-wetterstationen[!is.na(wetterstationen$Tmin),]
weather_summary<-wetterstationen_1%>%
  group_by(Year, JDay)%>%
  summarise(n_Sationen=length(unique(Station_ID)))

wetterstationen_beispieltag<-subset(wetterstationen_1, wetterstationen_1$Year=="2022"&wetterstationen_1$JDay=="1")


library(png)
library(grid)
img_p1<-readPNG("data/Flower_Cluster_p1.png")
img_p2<-readPNG("data/Thermometer.png")

dem_test<-raster("data/srtm_germany_dsm.tif")  #Quelle: https://www.opendem.info/download_srtm.html

dem_test_2<-mask(dem_test, DE_Bundeslaender_ohne_Meer_EPSG_4326_poly)

#saveRDS(dem_test_2, "dem_plots.RDS")
dem_test_2<-readRDS("dem_plots.RDS")

img_p1<-readPNG("data/Koenigsbluete.png")
img_p2<-readPNG("data/Thermometer.png")

# Define coordinates where you want to place the image
x_min <- 0.1  # Adjust based on your spatial extent
x_max <- 0.2
y_min <- 0.85
y_max <- 0.95

A1<-rasterVis::levelplot(dem_test_2$srtm_germany_dsm,
                     margin=F,
                     col.regions=terrain.colors(100),
                     #col.regions=viridis(100, direction = -1),
                     at=seq(0, 1500, length.out=100),
                     #main="quantile 10",
                     colorkey=F,
                     #scales=list(x=list(draw=FALSE)),
                     xlab="Longitude",
                     ylab="Latitude")+
  latticeExtra::layer(panel.points(map_data_red$geograph.Laenge,map_data_red$geograph.Breite, pch =21, col = "black", fill="black", cex=0.25))+
  latticeExtra::layer(
    grid.raster(img_p2, 
                x = mean(c(x_min, x_max)),   # Center of the image
                y = mean(c(y_min, y_max)),   # Center of the image
                width = unit(x_max - x_min, "native"), 
                height = unit(y_max - y_min, "native"))
  ) +
  latticeExtra::layer(
    grid.raster(img_p1, 
                x = mean(c(x_min, x_max)),   # Center of the image
                y = mean(c(y_min, y_max)),   # Center of the image
                width = 0.2, 
                height = 0.2)
  )



#apple growing regins
Standort<-c("Jork", "Tettnang", "Meckenheim", "Dresden")
Region<-c("Altes Land", "Lake\nConstance", "Rhineland", "Saxony")
Breite<-c(53.53287,47.670833,50.633333,51.049259)
Laenge<-c(9.68069,9.5875,7.016667,13.73836)

Anbaugebiete<-data.frame(Standort, Region, Breite, Laenge)


C4<-rasterVis::levelplot(dem_test_2$srtm_germany_dsm,
                         margin=F,
                         col.regions=terrain.colors(100),
                         #col.regions=viridis(100, direction = -1),
                         at=seq(0, 1500, length.out=100),
                         #main="quantile 10",
                         #colorkey=T,
                         colorkey=list(title=list("Elevation\n[m a.s.l]",fontsize=8)),
                         #scales=list(x=list(draw=FALSE)),
                         xlab="Longitude",
                         ylab="Latitude")+
  latticeExtra::layer({
    SpatialPolygonsRescale(layout.north.arrow(type = 1),
                           offset = c(14,48),
                           scale = 1.5)
  })+
  latticeExtra::layer({
    xs <- seq(12.5, 14.9, by=0.7)
    grid.rect(x=xs, y=47.35,
              width=0.7, height=0.1,
              gp=gpar(fill=rep(c('transparent', 'black'), 2)),
              default.units='native')
    grid.text(x= xs - 0.25, y=47.2, seq(0, 200, by=50),
              gp=gpar(cex=0.5), rot=0,
              default.units='native')
  })+
  latticeExtra::layer({
    grid.text(x= 12.5, y=47.1, "kilometers",
              gp=gpar(cex=0.5), rot=0,
              default.units='native',
              just="left")
  }) +
  latticeExtra::layer(
    grid.raster(img_p2, 
                x = mean(c(x_min, x_max)),   # Center of the image
                y = mean(c(y_min, y_max)),   # Center of the image
                width = 0.2, 
                height = 0.2)
  )+
  latticeExtra::layer({
    grid.text(x= Anbaugebiete$Laenge[1], y=Anbaugebiete$Breite[1], Anbaugebiete$Region[1],
              gp=gpar(cex=0.8, fontface= "bold", col="grey20"), rot=0,
              default.units='native',
              just="centre")})+
  latticeExtra::layer({
    grid.text(x= Anbaugebiete$Laenge[2], y=Anbaugebiete$Breite[2], Anbaugebiete$Region[2],
              gp=gpar(cex=0.8, fontface= "bold", col="grey20"), rot=0,
              default.units='native',
              just="centre")})+
  latticeExtra::layer({
    grid.text(x= Anbaugebiete$Laenge[3], y=Anbaugebiete$Breite[3], Anbaugebiete$Region[3],
              gp=gpar(cex=0.8, fontface= "bold", col="grey20"), rot=115,
              default.units='native',
              just="centre")})+
  latticeExtra::layer({
    grid.text(x= Anbaugebiete$Laenge[4], y=Anbaugebiete$Breite[4], Anbaugebiete$Region[4],
              gp=gpar(cex=0.8, fontface= "bold", col="grey20"), rot=0,
              default.units='native',
              just="centre")})+
  latticeExtra::layer(panel.points(wetterstationen_beispieltag$Länge, wetterstationen_beispieltag$Breite, pch =21, col = "black", fill="black", cex=0.25))
  
C4
png("figures/Anbaugebiete_Option_4.png", pointsize=8, width=5700, height=3800, res=600)
stations_all_plot <- c(A1,C4, layout = c(2,1), merge.legends = T)
print(stations_all_plot)
update(stations_all_plot, scales = list(alternating = 1))
dev.off()


#basic summaries####

summary(raster_early_ssp126_2050_median$var1.pred_total)
summary(raster_early_ssp245_2050_median$var1.pred_total)
summary(raster_early_ssp370_2050_median$var1.pred_total)
summary(raster_early_ssp585_2050_median$var1.pred_total)

summary(raster_early_ssp126_2085_median$var1.pred_total)
summary(raster_early_ssp245_2085_median$var1.pred_total)
summary(raster_early_ssp370_2085_median$var1.pred_total)
summary(raster_early_ssp585_2085_median$var1.pred_total)


summary(raster_late_ssp126_2050_median$var1.pred_total)
summary(raster_late_ssp245_2050_median$var1.pred_total)
summary(raster_late_ssp370_2050_median$var1.pred_total)
summary(raster_late_ssp585_2050_median$var1.pred_total)

summary(raster_late_ssp126_2085_median$var1.pred_total)
summary(raster_late_ssp245_2085_median$var1.pred_total)
summary(raster_late_ssp370_2085_median$var1.pred_total)
summary(raster_late_ssp585_2085_median$var1.pred_total)


summary(raster_early_ssp370_2085_quantile90)
summary(raster_early_ssp370_2085_quantile10)
summary(raster_early_ssp370_2085_median)