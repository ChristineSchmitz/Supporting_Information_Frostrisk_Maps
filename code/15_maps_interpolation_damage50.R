#Load libraries####
library(sf)#
library(stars)
library(tidyverse)#
library(gstat)
library(chillR)#
library(raster)#
library(sqldf)
library(terra)
library(scam)#
library(viridis)#
library(rasterVis)#
library(R.utils)
library(ggspatial)
library(sp)


#Settings####
Sys.setenv(lang = "en_US")

#Load data####
map_data_all<-read.csv("data/frost_damage_summary_new.csv")
map_data_early<-subset(map_data_all, map_data_all$ripening=="early")
all_pheno_stations<-read.csv("data/all_pheno_stations.csv")
new_names<-c(location="Stationsname", Station_id="Stations_id")
all_pheno_stations<-rename(all_pheno_stations, all_of(new_names) )
map_data_early<-merge(map_data_early, all_pheno_stations[2:6], by="Station_id")

gdf <- st_as_sf(map_data_early, coords=c("geograph.Laenge","geograph.Breite"), crs=4326)

# Define a regular grid
xmin <- min(st_coordinates(gdf$geometry)[,1])-0.5
xmax <- max(st_coordinates(gdf$geometry)[,1])+0.5
ymin <- min(st_coordinates(gdf$geometry)[,2])-0.5
ymax <- max(st_coordinates(gdf$geometry)[,2])+0.5
cell_size <- 0.05

dem<-rast("data/srtm_germany_dsm.tif")  #Quelle: https://www.opendem.info/download_srtm.html
dem <- project(dem, "EPSG:4326")

map_files_rds<-c(list.files("data/map_preparation", pattern = ".rds"))
map_files_all<-c(list.files("data/map_preparation"))
map_files<-setdiff(map_files_all,map_files_rds)

#Interpolation####
for (i in map_files){
  map_data<-read.csv(paste0("data/map_preparation/",i))
  for (j in c(2050,2085)){
    map_data_simyear<-subset(map_data, map_data$simyear== j)
    for (k in c("quantile10", "median", "quantile90")){
      map_data_simyear_model<-subset(map_data_simyear, map_data_simyear$model==k)
      gdf <- st_as_sf(map_data_simyear_model, coords=c("geograph.Laenge","geograph.Breite"), crs=4326)
      gdf$dem <- terra::extract(dem, vect(gdf$geometry))$srtm_germany_dsm
      
      
      model_interpolation <- 
        tryCatch({
          withTimeout({ model_interpolation <- scam(n_years_damage_50~s(dem, k=4, bs = "mpi"), data=gdf)}, timeout=30 , cpu = Inf)
        }, error = function(e){
          if(grepl("reached elapsed time limit",e$message))
            model <- lm(n_years_damage_50~dem, data = gdf) else
              paste(e$message,"EXTRACTERROR")
        })
      
      # predict expected temperature based on elevation
      gdf$damage_trend <- predict(model_interpolation,list(dem=gdf$dem))
      
      
      # calculate residuals
      gdf$residuals <- gdf$n_years_damage_50 - gdf$damage_trend
      

      # drop NAs
      gdf <- gdf[!is.na(gdf$residuals),]
      # remove duplicates
      gdf<-gdf[!duplicated(gdf$geometry),]

      
      # calculate empirical variogram
      gpb <- gstat(formula=residuals~1, data=gdf)
      vgpb <- variogram(gpb, boundaries = c(0, 1:10*20, 300, 400, 500, 700))
      
      # fit variogram model
      vgmpb <- vgm(model = "Sph", nugget=T)
      
      vgmpb <- fit.variogram(vgpb, vgmpb, fit.method = 6)
      
      # plot variogram
      plot(vgpb, vgmpb, lwd=2)
      
      # create grid for kriging
      grid <- expand.grid(x = seq(from = xmin, to = xmax, by = cell_size),
                          y = seq(from = ymin, to = ymax, by = cell_size))
      grid <- st_as_sf(grid, coords=c("x","y"), crs=4326)
      # local kriging with 15 neighboring points based on variogram model
      krig_res <- krige(residuals~1, gdf, grid, nmax=15, vgmpb)
      
      #assign(paste0("krig_res_2022_",i),krig_res)
      # calculate feature prediction (based on only elevation) for each grid cell
      krig_res$dem <- terra::extract(dem, krig_res)$srtm_germany_dsm
      krig_res$trend <- predict(model_interpolation,list(dem=krig_res$dem))
      
      # for total prediction add feature prediction and spatial prediction
      krig_res$var1.pred_total <- krig_res$var1.pred + krig_res$trend
      #replace values >100 and <0
      krig_res$var1.pred_total[krig_res$var1.pred_total>100]<-100
      krig_res$var1.pred_total[krig_res$var1.pred_total<0]<-0
      
      # drop NAs
      krig_res <- krig_res[!is.na(krig_res$var1.pred_total),]
      
      ras<-raster(xmn=xmin-0.5*cell_size, xmx=xmax, ymn=ymin-0.5*cell_size, ymx=ymax,
                  crs="EPSG:4326",
                  resolution=c(cell_size,cell_size), vals=NA)
      krig_res_prep<-krig_res[,c(3,6)]
      krig_res_raster <- terra::rasterize(krig_res_prep, ras)
      
      filename_save<-paste0("data/map_preparation/",i,"_damage_50_",j,"_",k,".rds")
      saveRDS(krig_res_raster, filename_save)
    }
  }
}


crs <- st_crs("EPSG:4326")
read_sf("data/DE_VG5000.gpkg", layer="vg5000_lan")|> st_transform(crs) -> DE_Bundeslaender_EPSG_4326 #source: https://daten.gdz.bkg.bund.de/produkte/vg/vg5000_0101/aktuell/vg5000_01-01.utm32s.gpkg.ebenen.zip
DE_Bundeslaender_ohne_Meer_EPSG_4326<-subset(DE_Bundeslaender_EPSG_4326, DE_Bundeslaender_EPSG_4326$GF==9)

DE_Bundeslaender_ohne_Meer_EPSG_4326_poly<-as(DE_Bundeslaender_ohne_Meer_EPSG_4326,"Spatial")

#ssp 585 early####
raster_early_ssp585_2050_median<-readRDS("data/map_preparation/map_data_early_ssp585.csv_damage_50_2050_median.rds")
raster_early_ssp585_2050_quantile10<-readRDS("data/map_preparation/map_data_early_ssp585.csv_damage_50_2050_quantile10.rds")
raster_early_ssp585_2050_quantile90<-readRDS("data/map_preparation/map_data_early_ssp585.csv_damage_50_2050_quantile90.rds")
raster_early_ssp585_2085_median<-readRDS("data/map_preparation/map_data_early_ssp585.csv_damage_50_2085_median.rds")
raster_early_ssp585_2085_quantile10<-readRDS("data/map_preparation/map_data_early_ssp585.csv_damage_50_2085_quantile10.rds")
raster_early_ssp585_2085_quantile90<-readRDS("data/map_preparation/map_data_early_ssp585.csv_damage_50_2085_quantile90.rds")


raster_early_ssp585_2050_median<-mask(raster_early_ssp585_2050_median, DE_Bundeslaender_ohne_Meer_EPSG_4326_poly)
raster_early_ssp585_2050_quantile10<-mask(raster_early_ssp585_2050_quantile10, DE_Bundeslaender_ohne_Meer_EPSG_4326_poly)
raster_early_ssp585_2050_quantile90<-mask(raster_early_ssp585_2050_quantile90, DE_Bundeslaender_ohne_Meer_EPSG_4326_poly)
raster_early_ssp585_2085_median<-mask(raster_early_ssp585_2085_median, DE_Bundeslaender_ohne_Meer_EPSG_4326_poly)
raster_early_ssp585_2085_quantile10<-mask(raster_early_ssp585_2085_quantile10, DE_Bundeslaender_ohne_Meer_EPSG_4326_poly)
raster_early_ssp585_2085_quantile90<-mask(raster_early_ssp585_2085_quantile90, DE_Bundeslaender_ohne_Meer_EPSG_4326_poly)


A4<-rasterVis::levelplot(raster_early_ssp585_2050_quantile10$var1.pred_total,
                         margin=F,
                         #col.regions=turbo(100),
                         col.regions=viridis(100, direction = -1),
                         at=seq(0, 100, length.out=100),
                         #main="quantile 10",
                         colorkey=F,
                         #scales=list(x=list(draw=FALSE)),
                         xlab="Longitude",
                         ylab="Latitude")+
  latticeExtra::layer({
    grid.text(x= 5.9, y=54.8, "SSP5-8.5\noptimistic\n2050",
              gp=gpar(fontsize=8), rot=0,
              default.units='native',
              just="left")
  })
A4


B4<-rasterVis::levelplot(raster_early_ssp585_2050_median$var1.pred_total,margin=F,
                         #col.regions=turbo(100),
                         col.regions=viridis(100, direction = -1),
                         at=seq(0, 100, length.out=100),
                         #main="2008 to 2022",
                         xlab=NULL,
                         ylab=NULL,
                         colorkey=F)+
  latticeExtra::layer({
    grid.text(x= 5.9, y=54.8, "SSP5-8.5\naverage\n2050",
              gp=gpar(fontsize=8), rot=0,
              default.units='native',
              just="left")
  })
B4

B4m<-rasterVis::levelplot(raster_early_ssp585_2050_median$var1.pred_total,margin=F,
                          #col.regions=turbo(100),
                          col.regions=viridis(100, direction = -1),
                          at=seq(0, 100, length.out=100),
                          #main="2008 to 2022",
                          xlab=NULL,
                          ylab=NULL,
                          colorkey=F)+
  latticeExtra::layer({
    grid.text(x= 5.9, y=54.8, "SSP5-8.5",
              gp=gpar(fontsize=8), rot=0,
              default.units='native',
              just="left")
  })
B4m


C4<-rasterVis::levelplot(raster_early_ssp585_2050_quantile90$var1.pred_total,margin=F,
                         #col.regions=turbo(100),
                         col.regions=viridis(100, direction = -1),
                         at=seq(0, 100, length.out=100),
                         #main="2008 to 2022",
                         xlab=NULL,
                         ylab=NULL,
                         colorkey=F)+
  latticeExtra::layer({
    grid.text(x= 5.9, y=54.8, "SSP5-8.5\npessimistic\n2050",
              gp=gpar(fontsize=8), rot=0,
              default.units='native',
              just="left")
  })
C4

D4<-rasterVis::levelplot(raster_early_ssp585_2085_quantile10$var1.pred_total,margin=F,
                         #col.regions=turbo(100),
                         col.regions=viridis(100, direction = -1),
                         at=seq(0, 100, length.out=100),
                         #main="2008 to 2022",
                         xlab=NULL,
                         ylab=NULL,
                         colorkey=F)+
  latticeExtra::layer({
    grid.text(x= 5.9, y=54.8, "SSP5-8.5\noptimistic\n2085",
              gp=gpar(fontsize=8), rot=0,
              default.units='native',
              just="left")
  })
D4

E4<-rasterVis::levelplot(raster_early_ssp585_2085_median$var1.pred_total,margin=F,
                         #col.regions=turbo(100),
                         col.regions=viridis(100, direction = -1),
                         at=seq(0, 100, length.out=100),
                         #main="2008 to 2022",
                         xlab=NULL,
                         ylab=NULL,
                         colorkey=F)+
  latticeExtra::layer({
    grid.text(x= 5.9, y=54.8, "SSP5-8.5\naverage\n2085",
              gp=gpar(fontsize=8), rot=0,
              default.units='native',
              just="left")
  })
E4

E4l<-rasterVis::levelplot(raster_early_ssp585_2085_median$var1.pred_total,margin=F,
                          #col.regions=turbo(100),
                          col.regions=viridis(100, direction = -1),
                          at=seq(0, 100, length.out=100),
                          #main="2008 to 2022",
                          xlab="Longitude",
                          ylab="Latitude",
                          colorkey=list(title=list("Probability\nof frost\ndamage\n>50 %\n[%]",fontsize=8))) +
  latticeExtra::layer({
    grid.text(x= 5.9, y=54.8, "SSP5-8.5",
              gp=gpar(fontsize=8), rot=0,
              default.units='native',
              just="left")
  })+
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
    grid.text(x= 12.5, y=47.05, "kilometers",
              gp=gpar(cex=0.5), rot=0,
              default.units='native',
              just="left")
  })
E4l

F4<-rasterVis::levelplot(raster_early_ssp585_2085_quantile90$var1.pred_total,margin=F,
                         #col.regions=turbo(100),
                         col.regions=viridis(100, direction = -1),
                         at=seq(0, 100, length.out=100),
                         #main="2008 to 2022",
                         #scales=list(y=list(draw=FALSE)),
                         ylab=NULL,
                         xlab=NULL,
                         colorkey=list(title=list("Probability\nof frost\ndamage\n>50 %\n[%]",fontsize=8))) +
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
    grid.text(x= 12.5, y=47.05, "kilometers",
              gp=gpar(cex=0.5), rot=0,
              default.units='native',
              just="left")
  })+
  latticeExtra::layer({
    grid.text(x= 5.9, y=54.8, "SSP5-8.5\npessimistic\n2085",
              gp=gpar(fontsize=8), rot=0,
              default.units='native',
              just="left")
  })

F4

# Combination via `c.trellis`
ssp585_early_all <- c(A4,B4,C4,D4,E4,F4, layout = c(3,2), merge.legends = T)
print(ssp585_early_all)
update(ssp585_early_all, scales = list(alternating = 1))

png("new_names_ssp585_early_damage50.png", pointsize=8, width=5700, height=4600, res=600)
ssp585_early_all <- c(A4,B4,C4,D4,E4,F4, layout = c(3,2), merge.legends = T)
print(ssp585_early_all)
update(ssp585_early_all, scales = list(alternating = 1))
dev.off()


#ssp 126 early####
raster_early_ssp126_2050_median<-readRDS("data/map_preparation/map_data_early_ssp126.csv_damage_50_2050_median.rds")
raster_early_ssp126_2050_quantile10<-readRDS("data/map_preparation/map_data_early_ssp126.csv_damage_50_2050_quantile10.rds")
raster_early_ssp126_2050_quantile90<-readRDS("data/map_preparation/map_data_early_ssp126.csv_damage_50_2050_quantile90.rds")
raster_early_ssp126_2085_median<-readRDS("data/map_preparation/map_data_early_ssp126.csv_damage_50_2085_median.rds")
raster_early_ssp126_2085_quantile10<-readRDS("data/map_preparation/map_data_early_ssp126.csv_damage_50_2085_quantile10.rds")
raster_early_ssp126_2085_quantile90<-readRDS("data/map_preparation/map_data_early_ssp126.csv_damage_50_2085_quantile90.rds")


raster_early_ssp126_2050_median<-mask(raster_early_ssp126_2050_median, DE_Bundeslaender_ohne_Meer_EPSG_4326_poly)
raster_early_ssp126_2050_quantile10<-mask(raster_early_ssp126_2050_quantile10, DE_Bundeslaender_ohne_Meer_EPSG_4326_poly)
raster_early_ssp126_2050_quantile90<-mask(raster_early_ssp126_2050_quantile90, DE_Bundeslaender_ohne_Meer_EPSG_4326_poly)
raster_early_ssp126_2085_median<-mask(raster_early_ssp126_2085_median, DE_Bundeslaender_ohne_Meer_EPSG_4326_poly)
raster_early_ssp126_2085_quantile10<-mask(raster_early_ssp126_2085_quantile10, DE_Bundeslaender_ohne_Meer_EPSG_4326_poly)
raster_early_ssp126_2085_quantile90<-mask(raster_early_ssp126_2085_quantile90, DE_Bundeslaender_ohne_Meer_EPSG_4326_poly)



A1<-rasterVis::levelplot(raster_early_ssp126_2050_quantile10$var1.pred_total,
                         margin=F,
                         #col.regions=turbo(100),
                         col.regions=viridis(100, direction = -1),
                         at=seq(0, 100, length.out=100),
                         #main="quantile 10",
                         colorkey=F,
                         #scales=list(x=list(draw=FALSE)),
                         xlab="Longitude",
                         ylab="Latitude")+
  latticeExtra::layer({
    grid.text(x= 5.9, y=54.8, "SSP1-2.6\noptimistic\n2050",
              gp=gpar(fontsize=8), rot=0,
              default.units='native',
              just="left")
  })
A1


B1<-rasterVis::levelplot(raster_early_ssp126_2050_median$var1.pred_total,margin=F,
                         #col.regions=turbo(100),
                         col.regions=viridis(100, direction = -1),
                         at=seq(0, 100, length.out=100),
                         #main="2050",
                         xlab="Longitude",
                         ylab="Latitude",
                         colorkey=F)+
  latticeExtra::layer({
    grid.text(x= 5.9, y=54.8, "SSP1-2.6\naverage\n2050",
              gp=gpar(fontsize=8), rot=0,
              default.units='native',
              just="left")
  })
B1
B1m<-rasterVis::levelplot(raster_early_ssp126_2050_median$var1.pred_total,margin=F,
                          #col.regions=turbo(100),
                          col.regions=viridis(100, direction = -1),
                          at=seq(0, 100, length.out=100),
                          #main="2050",
                          xlab="Longitude",
                          ylab="Latitude",
                          colorkey=F)+
  latticeExtra::layer({
    grid.text(x= 5.9, y=54.8, "SSP1-2.6",
              gp=gpar(fontsize=8), rot=0,
              default.units='native',
              just="left")
  })
B1m

C1<-rasterVis::levelplot(raster_early_ssp126_2050_quantile90$var1.pred_total,margin=F,
                         #col.regions=turbo(100),
                         col.regions=viridis(100, direction = -1),
                         at=seq(0, 100, length.out=100),
                         #main="2008 to 2022",
                         xlab=NULL,
                         ylab=NULL,
                         colorkey=F)+
  latticeExtra::layer({
    grid.text(x= 5.9, y=54.8, "SSP1-2.6\npessimistic\n2050",
              gp=gpar(fontsize=8), rot=0,
              default.units='native',
              just="left")
  })
C1

D1<-rasterVis::levelplot(raster_early_ssp126_2085_quantile10$var1.pred_total,margin=F,
                         #col.regions=turbo(100),
                         col.regions=viridis(100, direction = -1),
                         at=seq(0, 100, length.out=100),
                         #main="2008 to 2022",
                         xlab=NULL,
                         ylab=NULL,
                         colorkey=F)+
  latticeExtra::layer({
    grid.text(x= 5.9, y=54.8, "SSP1-2.6\noptimistic\n2085",
              gp=gpar(fontsize=8), rot=0,
              default.units='native',
              just="left")
  })
D1

E1<-rasterVis::levelplot(raster_early_ssp126_2085_median$var1.pred_total,margin=F,
                         #col.regions=turbo(100),
                         col.regions=viridis(100, direction = -1),
                         at=seq(0, 100, length.out=100),
                         #main="2085",
                         xlab=NULL,
                         ylab=NULL,
                         colorkey=F)+
  latticeExtra::layer({
    grid.text(x= 5.9, y=54.8, "SSP1-2.6\naverage\n2085",
              gp=gpar(fontsize=8), rot=0,
              default.units='native',
              just="left")
  })
E1

E1m<-rasterVis::levelplot(raster_early_ssp126_2085_median$var1.pred_total,margin=F,
                          #col.regions=turbo(100),
                          col.regions=viridis(100, direction = -1),
                          at=seq(0, 100, length.out=100),
                          #main="2085",
                          xlab=NULL,
                          ylab=NULL,
                          colorkey=F)+
  latticeExtra::layer({
    grid.text(x= 5.9, y=54.8, "SSP1-2.6",
              gp=gpar(fontsize=8), rot=0,
              default.units='native',
              just="left")
  })
E1m

F1<-rasterVis::levelplot(raster_early_ssp126_2085_quantile90$var1.pred_total,margin=F,
                         #col.regions=turbo(100),
                         col.regions=viridis(100, direction = -1),
                         at=seq(0, 100, length.out=100),
                         #main="2008 to 2022",
                         #scales=list(y=list(draw=FALSE)),
                         ylab=NULL,
                         xlab=NULL,
                         colorkey=list(title=list("Probability\nof frost\ndamage\n>50 %\n[%]",fontsize=8))) +
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
    grid.text(x= 12.5, y=47.05, "kilometers",
              gp=gpar(cex=0.5), rot=0,
              default.units='native',
              just="left")
  })+
  latticeExtra::layer({
    grid.text(x= 5.9, y=54.8, "SSP1-2.6\npessimistic\n2085",
              gp=gpar(fontsize=8), rot=0,
              default.units='native',
              just="left")
  })

F1

# Combination via `c.trellis`
ssp126_early_all <- c(A1,B1,C1,D1,E1,F1, layout = c(3,2), merge.legends = T)
print(ssp126_early_all)
update(ssp126_early_all, scales = list(alternating = 1))

png("new_names_ssp126_early_damage50.png", pointsize=8, width=5700, height=4600, res=600)
ssp126_early_all <- c(A1,B1,C1,D1,E1,F1, layout = c(3,2), merge.legends = T)
print(ssp126_early_all)
update(ssp126_early_all, scales = list(alternating = 1))
dev.off()


#ssp 245 early####
raster_early_ssp245_2050_median<-readRDS("data/map_preparation/map_data_early_ssp245.csv_damage_50_2050_median.rds")
raster_early_ssp245_2050_quantile10<-readRDS("data/map_preparation/map_data_early_ssp245.csv_damage_50_2050_quantile10.rds")
raster_early_ssp245_2050_quantile90<-readRDS("data/map_preparation/map_data_early_ssp245.csv_damage_50_2050_quantile90.rds")
raster_early_ssp245_2085_median<-readRDS("data/map_preparation/map_data_early_ssp245.csv_damage_50_2085_median.rds")
raster_early_ssp245_2085_quantile10<-readRDS("data/map_preparation/map_data_early_ssp245.csv_damage_50_2085_quantile10.rds")
raster_early_ssp245_2085_quantile90<-readRDS("data/map_preparation/map_data_early_ssp245.csv_damage_50_2085_quantile90.rds")


raster_early_ssp245_2050_median<-mask(raster_early_ssp245_2050_median, DE_Bundeslaender_ohne_Meer_EPSG_4326_poly)
raster_early_ssp245_2050_quantile10<-mask(raster_early_ssp245_2050_quantile10, DE_Bundeslaender_ohne_Meer_EPSG_4326_poly)
raster_early_ssp245_2050_quantile90<-mask(raster_early_ssp245_2050_quantile90, DE_Bundeslaender_ohne_Meer_EPSG_4326_poly)
raster_early_ssp245_2085_median<-mask(raster_early_ssp245_2085_median, DE_Bundeslaender_ohne_Meer_EPSG_4326_poly)
raster_early_ssp245_2085_quantile10<-mask(raster_early_ssp245_2085_quantile10, DE_Bundeslaender_ohne_Meer_EPSG_4326_poly)
raster_early_ssp245_2085_quantile90<-mask(raster_early_ssp245_2085_quantile90, DE_Bundeslaender_ohne_Meer_EPSG_4326_poly)



A2<-rasterVis::levelplot(raster_early_ssp245_2050_quantile10$var1.pred_total,
                         margin=F,
                         #col.regions=turbo(100),
                         col.regions=viridis(100, direction = -1),
                         at=seq(0, 100, length.out=100),
                         #main="quantile 10",
                         colorkey=F,
                         #scales=list(x=list(draw=FALSE)),
                         xlab="Longitude",
                         ylab="Latitude")+
  latticeExtra::layer({
    grid.text(x= 5.9, y=54.8, "SSP2-4.5\noptimistic\n2050",
              gp=gpar(fontsize=8), rot=0,
              default.units='native',
              just="left")
  })
A2


B2<-rasterVis::levelplot(raster_early_ssp245_2050_median$var1.pred_total,margin=F,
                         #col.regions=turbo(100),
                         col.regions=viridis(100, direction = -1),
                         at=seq(0, 100, length.out=100),
                         #main="2008 to 2022",
                         xlab=NULL,
                         ylab=NULL,
                         colorkey=F)+
  latticeExtra::layer({
    grid.text(x= 5.9, y=54.8, "SSP2-4.5\naverage\n2050",
              gp=gpar(fontsize=8), rot=0,
              default.units='native',
              just="left")
  })
B2

B2m<-rasterVis::levelplot(raster_early_ssp245_2050_median$var1.pred_total,margin=F,
                          #col.regions=turbo(100),
                          col.regions=viridis(100, direction = -1),
                          at=seq(0, 100, length.out=100),
                          #main="2008 to 2022",
                          xlab=NULL,
                          ylab=NULL,
                          colorkey=F)+
  latticeExtra::layer({
    grid.text(x= 5.9, y=54.8, "SSP2-4.5",
              gp=gpar(fontsize=8), rot=0,
              default.units='native',
              just="left")
  })
B2m

C2<-rasterVis::levelplot(raster_early_ssp245_2050_quantile90$var1.pred_total,margin=F,
                         #col.regions=turbo(100),
                         col.regions=viridis(100, direction = -1),
                         at=seq(0, 100, length.out=100),
                         #main="2008 to 2022",
                         xlab=NULL,
                         ylab=NULL,
                         colorkey=F)+
  latticeExtra::layer({
    grid.text(x= 5.9, y=54.8, "SSP2-4.5\npessimistic\n2050",
              gp=gpar(fontsize=8), rot=0,
              default.units='native',
              just="left")
  })
C2

D2<-rasterVis::levelplot(raster_early_ssp245_2085_quantile10$var1.pred_total,margin=F,
                         #col.regions=turbo(100),
                         col.regions=viridis(100, direction = -1),
                         at=seq(0, 100, length.out=100),
                         #main="2008 to 2022",
                         xlab=NULL,
                         ylab=NULL,
                         colorkey=F)+
  latticeExtra::layer({
    grid.text(x= 5.9, y=54.8, "SSP2-4.5\noptimistic\n2085",
              gp=gpar(fontsize=8), rot=0,
              default.units='native',
              just="left")
  })
D2

E2<-rasterVis::levelplot(raster_early_ssp245_2085_median$var1.pred_total,margin=F,
                         #col.regions=turbo(100),
                         col.regions=viridis(100, direction = -1),
                         at=seq(0, 100, length.out=100),
                         #main="2008 to 2022",
                         xlab=NULL,
                         ylab=NULL,
                         colorkey=F)+
  latticeExtra::layer({
    grid.text(x= 5.9, y=54.8, "SSP2-4.5\naverage\n2085",
              gp=gpar(fontsize=8), rot=0,
              default.units='native',
              just="left")
  })
E2
E2m<-rasterVis::levelplot(raster_early_ssp245_2085_median$var1.pred_total,margin=F,
                          #col.regions=turbo(100),
                          col.regions=viridis(100, direction = -1),
                          at=seq(0, 100, length.out=100),
                          #main="2008 to 2022",
                          xlab=NULL,
                          ylab=NULL,
                          colorkey=F)+
  latticeExtra::layer({
    grid.text(x= 5.9, y=54.8, "SSP2-4.5",
              gp=gpar(fontsize=8), rot=0,
              default.units='native',
              just="left")
  })
E2m

F2<-rasterVis::levelplot(raster_early_ssp245_2085_quantile90$var1.pred_total,margin=F,
                         #col.regions=turbo(100),
                         col.regions=viridis(100, direction = -1),
                         at=seq(0, 100, length.out=100),
                         #main="2008 to 2022",
                         #scales=list(y=list(draw=FALSE)),
                         ylab=NULL,
                         xlab=NULL,
                         colorkey=list(title=list("Probability\nof frost\ndamage\n>50 %\n[%]",fontsize=8))) +
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
    grid.text(x= 12.5, y=47.05, "kilometers",
              gp=gpar(cex=0.5), rot=0,
              default.units='native',
              just="left")
  })+
  latticeExtra::layer({
    grid.text(x= 5.9, y=54.8, "SSP2-4.5\npessimistic\n2085",
              gp=gpar(fontsize=8), rot=0,
              default.units='native',
              just="left")
  })

F2
# Combination via `c.trellis`
ssp245_early_all <- c(A2,B2,C2,D2,E2,F2, layout = c(3,2), merge.legends = T)
print(ssp245_early_all)
update(ssp245_early_all, scales = list(alternating = 1))

png("new_names_ssp245_early_damage50.png", pointsize=8, width=5700, height=4600, res=600)
ssp245_early_all <- c(A2,B2,C2,D2,E2,F2, layout = c(3,2), merge.legends = T)
print(ssp245_early_all)
update(ssp245_early_all, scales = list(alternating = 1))
dev.off()

#ssp 370 early####
raster_early_ssp370_2050_median<-readRDS("data/map_preparation/map_data_early_ssp370.csv_damage_50_2050_median.rds")
raster_early_ssp370_2050_quantile10<-readRDS("data/map_preparation/map_data_early_ssp370.csv_damage_50_2050_quantile10.rds")
raster_early_ssp370_2050_quantile90<-readRDS("data/map_preparation/map_data_early_ssp370.csv_damage_50_2050_quantile90.rds")
raster_early_ssp370_2085_median<-readRDS("data/map_preparation/map_data_early_ssp370.csv_damage_50_2085_median.rds")
raster_early_ssp370_2085_quantile10<-readRDS("data/map_preparation/map_data_early_ssp370.csv_damage_50_2085_quantile10.rds")
raster_early_ssp370_2085_quantile90<-readRDS("data/map_preparation/map_data_early_ssp370.csv_damage_50_2085_quantile90.rds")


raster_early_ssp370_2050_median<-mask(raster_early_ssp370_2050_median, DE_Bundeslaender_ohne_Meer_EPSG_4326_poly)
raster_early_ssp370_2050_quantile10<-mask(raster_early_ssp370_2050_quantile10, DE_Bundeslaender_ohne_Meer_EPSG_4326_poly)
raster_early_ssp370_2050_quantile90<-mask(raster_early_ssp370_2050_quantile90, DE_Bundeslaender_ohne_Meer_EPSG_4326_poly)
raster_early_ssp370_2085_median<-mask(raster_early_ssp370_2085_median, DE_Bundeslaender_ohne_Meer_EPSG_4326_poly)
raster_early_ssp370_2085_quantile10<-mask(raster_early_ssp370_2085_quantile10, DE_Bundeslaender_ohne_Meer_EPSG_4326_poly)
raster_early_ssp370_2085_quantile90<-mask(raster_early_ssp370_2085_quantile90, DE_Bundeslaender_ohne_Meer_EPSG_4326_poly)



A3<-rasterVis::levelplot(raster_early_ssp370_2050_quantile10$var1.pred_total,
                         margin=F,
                         #col.regions=turbo(100),
                         col.regions=viridis(100, direction = -1),
                         at=seq(0, 100, length.out=100),
                         #main="quantile 10",
                         colorkey=F,
                         #scales=list(x=list(draw=FALSE)),
                         xlab="Longitude",
                         ylab="Latitude")+
  latticeExtra::layer({
    grid.text(x= 5.9, y=54.8, "SSP3-7.0\noptimistic\n2050",
              gp=gpar(fontsize=8), rot=0,
              default.units='native',
              just="left")
  })
A3


B3<-rasterVis::levelplot(raster_early_ssp370_2050_median$var1.pred_total,margin=F,
                         #col.regions=turbo(100),
                         col.regions=viridis(100, direction = -1),
                         at=seq(0, 100, length.out=100),
                         #main="2008 to 2022",
                         xlab=NULL,
                         ylab=NULL,
                         colorkey=F)+
  latticeExtra::layer({
    grid.text(x= 5.9, y=54.8, "SSP3-7.0\naverage\n2050",
              gp=gpar(fontsize=8), rot=0,
              default.units='native',
              just="left")
  })
B3

B3m<-rasterVis::levelplot(raster_early_ssp370_2050_median$var1.pred_total,margin=F,
                          #col.regions=turbo(100),
                          col.regions=viridis(100, direction = -1),
                          at=seq(0, 100, length.out=100),
                          #main="2008 to 2022",
                          xlab=NULL,
                          ylab=NULL,
                          colorkey=F)+
  latticeExtra::layer({
    grid.text(x= 5.9, y=54.8, "SSP3-7.0",
              gp=gpar(fontsize=8), rot=0,
              default.units='native',
              just="left")
  })
B3m

C3<-rasterVis::levelplot(raster_early_ssp370_2050_quantile90$var1.pred_total,margin=F,
                         #col.regions=turbo(100),
                         col.regions=viridis(100, direction = -1),
                         at=seq(0, 100, length.out=100),
                         #main="2008 to 2022",
                         xlab=NULL,
                         ylab=NULL,
                         colorkey=F)+
  latticeExtra::layer({
    grid.text(x= 5.9, y=54.8, "SSP3-7.0\npessimistic\n2050",
              gp=gpar(fontsize=8), rot=0,
              default.units='native',
              just="left")
  })
C3

D3<-rasterVis::levelplot(raster_early_ssp370_2085_quantile10$var1.pred_total,margin=F,
                         #col.regions=turbo(100),
                         col.regions=viridis(100, direction = -1),
                         at=seq(0, 100, length.out=100),
                         #main="2008 to 2022",
                         xlab=NULL,
                         ylab=NULL,
                         colorkey=F)+
  latticeExtra::layer({
    grid.text(x= 5.9, y=54.8, "SSP3-7.0\noptimistic\n2085",
              gp=gpar(fontsize=8), rot=0,
              default.units='native',
              just="left")
  })
D3

E3<-rasterVis::levelplot(raster_early_ssp370_2085_median$var1.pred_total,margin=F,
                         #col.regions=turbo(100),
                         col.regions=viridis(100, direction = -1),
                         at=seq(0, 100, length.out=100),
                         #main="2008 to 2022",
                         xlab=NULL,
                         ylab=NULL,
                         colorkey=F)+
  latticeExtra::layer({
    grid.text(x= 5.9, y=54.8, "SSP3-7.0\naverage\n2085",
              gp=gpar(fontsize=8), rot=0,
              default.units='native',
              just="left")
  })
E3

E3m<-rasterVis::levelplot(raster_early_ssp370_2085_median$var1.pred_total,margin=F,
                          #col.regions=turbo(100),
                          col.regions=viridis(100, direction = -1),
                          at=seq(0, 100, length.out=100),
                          #main="2008 to 2022",
                          xlab=NULL,
                          ylab=NULL,
                          colorkey=F)+
  latticeExtra::layer({
    grid.text(x= 5.9, y=54.8, "SSP3-7.0",
              gp=gpar(fontsize=8), rot=0,
              default.units='native',
              just="left")
  })
E3m

F3<-rasterVis::levelplot(raster_early_ssp370_2085_quantile90$var1.pred_total,margin=F,
                         #col.regions=turbo(100),
                         col.regions=viridis(100, direction = -1),
                         at=seq(0, 100, length.out=100),
                         #main="2008 to 2022",
                         #scales=list(y=list(draw=FALSE)),
                         ylab=NULL,
                         xlab=NULL,
                         colorkey=list(title=list("Probability\nof frost\ndamage\n>50 %\n[%]",fontsize=8))) +
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
    grid.text(x= 12.5, y=47.05, "kilometers",
              gp=gpar(cex=0.5), rot=0,
              default.units='native',
              just="left")
  })+
  latticeExtra::layer({
    grid.text(x= 5.9, y=54.8, "SSP3-7.0\npessimistic\n2085",
              gp=gpar(fontsize=8), rot=0,
              default.units='native',
              just="left")
  })

F3

# Combination via `c.trellis`
ssp370_early_all <- c(A3,B3,C3,D3,E3,F3, layout = c(3,2), merge.legends = T)
print(ssp370_early_all)
update(ssp370_early_all, scales = list(alternating = 1))

png("new_names_ssp370_early_damage50.png", pointsize=8, width=5700, height=4600, res=600)
ssp370_early_all <- c(A3,B3,C3,D3,E3,F3, layout = c(3,2), merge.legends = T)
print(ssp370_early_all)
update(ssp370_early_all, scales = list(alternating = 1))
dev.off()

median_all_ssp<-c(B1m,E1m,B2m,E2m,B3m,E3m,B4m,E4l, layout=c(2,4), merge.legends=T)
print(median_all_ssp)
update(median_all_ssp, scales = list(alternating = 1))

library(grid)

png("new_names_median_all_ssp_early_damage50.png", pointsize=8, width=3600, height=8000, res=600)
median_all_ssp<-c(B1m,E1m,B2m,E2m,B3m,E3m,B4m,E4l, layout=c(2,4), merge.legends=T)
print(median_all_ssp)

#print(median_all_ssp, position = c(0, 0, 1, 1), more = TRUE)
update(median_all_ssp, scales = list(alternating = 1))
# Add column titles manually
grid.text(label = c("2050", "2085"), 
          x = seq(0.325, 0.7, length.out = 2),  # Adjust positions for column headers
          y = 0.98, gp = gpar(fontsize = 14, fontface = "bold"))
dev.off()



#ssp 585 late####
raster_late_ssp585_2050_median<-readRDS("data/map_preparation/map_data_late_ssp585.csv_damage_50_2050_median.rds")
raster_late_ssp585_2050_quantile10<-readRDS("data/map_preparation/map_data_late_ssp585.csv_damage_50_2050_quantile10.rds")
raster_late_ssp585_2050_quantile90<-readRDS("data/map_preparation/map_data_late_ssp585.csv_damage_50_2050_quantile90.rds")
raster_late_ssp585_2085_median<-readRDS("data/map_preparation/map_data_late_ssp585.csv_damage_50_2085_median.rds")
raster_late_ssp585_2085_quantile10<-readRDS("data/map_preparation/map_data_late_ssp585.csv_damage_50_2085_quantile10.rds")
raster_late_ssp585_2085_quantile90<-readRDS("data/map_preparation/map_data_late_ssp585.csv_damage_50_2085_quantile90.rds")


raster_late_ssp585_2050_median<-mask(raster_late_ssp585_2050_median, DE_Bundeslaender_ohne_Meer_EPSG_4326_poly)
raster_late_ssp585_2050_quantile10<-mask(raster_late_ssp585_2050_quantile10, DE_Bundeslaender_ohne_Meer_EPSG_4326_poly)
raster_late_ssp585_2050_quantile90<-mask(raster_late_ssp585_2050_quantile90, DE_Bundeslaender_ohne_Meer_EPSG_4326_poly)
raster_late_ssp585_2085_median<-mask(raster_late_ssp585_2085_median, DE_Bundeslaender_ohne_Meer_EPSG_4326_poly)
raster_late_ssp585_2085_quantile10<-mask(raster_late_ssp585_2085_quantile10, DE_Bundeslaender_ohne_Meer_EPSG_4326_poly)
raster_late_ssp585_2085_quantile90<-mask(raster_late_ssp585_2085_quantile90, DE_Bundeslaender_ohne_Meer_EPSG_4326_poly)



A1<-rasterVis::levelplot(raster_late_ssp585_2050_quantile10$var1.pred_total,
                         margin=F,
                         #col.regions=turbo(100),
                         col.regions=viridis(100, direction = -1),
                         at=seq(0, 100, length.out=100),
                         #main="quantile 10",
                         colorkey=F,
                         #scales=list(x=list(draw=FALSE)),
                         xlab="Longitude",
                         ylab="Latitude")+
  latticeExtra::layer({
    grid.text(x= 5.9, y=54.8, "SSP5-8.5\noptimistic\n2050",
              gp=gpar(fontsize=8), rot=0,
              default.units='native',
              just="left")
  })
A1


B1<-rasterVis::levelplot(raster_late_ssp585_2050_median$var1.pred_total,margin=F,
                         #col.regions=turbo(100),
                         col.regions=viridis(100, direction = -1),
                         at=seq(0, 100, length.out=100),
                         #main="2008 to 2022",
                         xlab=NULL,
                         ylab=NULL,
                         colorkey=F)+
  latticeExtra::layer({
    grid.text(x= 5.9, y=54.8, "SSP5-8.5\naverage\n2050",
              gp=gpar(fontsize=8), rot=0,
              default.units='native',
              just="left")
  })
B1

C1<-rasterVis::levelplot(raster_late_ssp585_2050_quantile90$var1.pred_total,margin=F,
                         #col.regions=turbo(100),
                         col.regions=viridis(100, direction = -1),
                         at=seq(0, 100, length.out=100),
                         #main="2008 to 2022",
                         xlab=NULL,
                         ylab=NULL,
                         colorkey=F)+
  latticeExtra::layer({
    grid.text(x= 5.9, y=54.8, "SSP5-8.5\npessimistic\n2050",
              gp=gpar(fontsize=8), rot=0,
              default.units='native',
              just="left")
  })
C1

D1<-rasterVis::levelplot(raster_late_ssp585_2085_quantile10$var1.pred_total,margin=F,
                         #col.regions=turbo(100),
                         col.regions=viridis(100, direction = -1),
                         at=seq(0, 100, length.out=100),
                         #main="2008 to 2022",
                         xlab=NULL,
                         ylab=NULL,
                         colorkey=F)+
  latticeExtra::layer({
    grid.text(x= 5.9, y=54.8, "SSP5-8.5\noptimistic\n2085",
              gp=gpar(fontsize=8), rot=0,
              default.units='native',
              just="left")
  })
D1

E1<-rasterVis::levelplot(raster_late_ssp585_2085_median$var1.pred_total,margin=F,
                         #col.regions=turbo(100),
                         col.regions=viridis(100, direction = -1),
                         at=seq(0, 100, length.out=100),
                         #main="2008 to 2022",
                         xlab=NULL,
                         ylab=NULL,
                         colorkey=F)+
  latticeExtra::layer({
    grid.text(x= 5.9, y=54.8, "SSP5-8.5\naverage\n2085",
              gp=gpar(fontsize=8), rot=0,
              default.units='native',
              just="left")
  })
E1

F1<-rasterVis::levelplot(raster_late_ssp585_2085_quantile90$var1.pred_total,margin=F,
                         #col.regions=turbo(100),
                         col.regions=viridis(100, direction = -1),
                         at=seq(0, 100, length.out=100),
                         #main="2008 to 2022",
                         #scales=list(y=list(draw=FALSE)),
                         ylab=NULL,
                         xlab=NULL,
                         colorkey=list(title=list("Probability\nof frost\ndamage\n>50 %\n[%]",fontsize=8))) +
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
    grid.text(x= 12.5, y=47.05, "kilometers",
              gp=gpar(cex=0.5), rot=0,
              default.units='native',
              just="left")
  })+
  latticeExtra::layer({
    grid.text(x= 5.9, y=54.8, "SSP5-8.5\npessimistic\n2085",
              gp=gpar(fontsize=8), rot=0,
              default.units='native',
              just="left")
  })

F1

# Combination via `c.trellis`
ssp585_late_all <- c(A1,B1,C1,D1,E1,F1, layout = c(3,2), merge.legends = T)
print(ssp585_late_all)
update(ssp585_late_all, scales = list(alternating = 1))

png("new_names_ssp585_late_damage50.png", pointsize=8, width=5700, height=4600, res=600)
ssp585_late_all <- c(A1,B1,C1,D1,E1,F1, layout = c(3,2), merge.legends = T)
print(ssp585_late_all)
update(ssp585_late_all, scales = list(alternating = 1))
dev.off()


#ssp 126 late####
raster_late_ssp126_2050_median<-readRDS("data/map_preparation/map_data_late_ssp126.csv_damage_50_2050_median.rds")
raster_late_ssp126_2050_quantile10<-readRDS("data/map_preparation/map_data_late_ssp126.csv_damage_50_2050_quantile10.rds")
raster_late_ssp126_2050_quantile90<-readRDS("data/map_preparation/map_data_late_ssp126.csv_damage_50_2050_quantile90.rds")
raster_late_ssp126_2085_median<-readRDS("data/map_preparation/map_data_late_ssp126.csv_damage_50_2085_median.rds")
raster_late_ssp126_2085_quantile10<-readRDS("data/map_preparation/map_data_late_ssp126.csv_damage_50_2085_quantile10.rds")
raster_late_ssp126_2085_quantile90<-readRDS("data/map_preparation/map_data_late_ssp126.csv_damage_50_2085_quantile90.rds")


raster_late_ssp126_2050_median<-mask(raster_late_ssp126_2050_median, DE_Bundeslaender_ohne_Meer_EPSG_4326_poly)
raster_late_ssp126_2050_quantile10<-mask(raster_late_ssp126_2050_quantile10, DE_Bundeslaender_ohne_Meer_EPSG_4326_poly)
raster_late_ssp126_2050_quantile90<-mask(raster_late_ssp126_2050_quantile90, DE_Bundeslaender_ohne_Meer_EPSG_4326_poly)
raster_late_ssp126_2085_median<-mask(raster_late_ssp126_2085_median, DE_Bundeslaender_ohne_Meer_EPSG_4326_poly)
raster_late_ssp126_2085_quantile10<-mask(raster_late_ssp126_2085_quantile10, DE_Bundeslaender_ohne_Meer_EPSG_4326_poly)
raster_late_ssp126_2085_quantile90<-mask(raster_late_ssp126_2085_quantile90, DE_Bundeslaender_ohne_Meer_EPSG_4326_poly)



A1<-rasterVis::levelplot(raster_late_ssp126_2050_quantile10$var1.pred_total,
                         margin=F,
                         #col.regions=turbo(100),
                         col.regions=viridis(100, direction = -1),
                         at=seq(0, 100, length.out=100),
                         #main="quantile 10",
                         colorkey=F,
                         #scales=list(x=list(draw=FALSE)),
                         xlab="Longitude",
                         ylab="Latitude")+
  latticeExtra::layer({
    grid.text(x= 5.9, y=54.8, "SSP1-2.6\noptimistic\n2050",
              gp=gpar(fontsize=8), rot=0,
              default.units='native',
              just="left")
  })
A1


B1<-rasterVis::levelplot(raster_late_ssp126_2050_median$var1.pred_total,margin=F,
                         #col.regions=turbo(100),
                         col.regions=viridis(100, direction = -1),
                         at=seq(0, 100, length.out=100),
                         #main="2008 to 2022",
                         xlab=NULL,
                         ylab=NULL,
                         colorkey=F)+
  latticeExtra::layer({
    grid.text(x= 5.9, y=54.8, "SSP1-2.6\naverage\n2050",
              gp=gpar(fontsize=8), rot=0,
              default.units='native',
              just="left")
  })
B1

C1<-rasterVis::levelplot(raster_late_ssp126_2050_quantile90$var1.pred_total,margin=F,
                         #col.regions=turbo(100),
                         col.regions=viridis(100, direction = -1),
                         at=seq(0, 100, length.out=100),
                         #main="2008 to 2022",
                         xlab=NULL,
                         ylab=NULL,
                         colorkey=F)+
  latticeExtra::layer({
    grid.text(x= 5.9, y=54.8, "SSP1-2.6\npessimistic\n2050",
              gp=gpar(fontsize=8), rot=0,
              default.units='native',
              just="left")
  })
C1

D1<-rasterVis::levelplot(raster_late_ssp126_2085_quantile10$var1.pred_total,margin=F,
                         #col.regions=turbo(100),
                         col.regions=viridis(100, direction = -1),
                         at=seq(0, 100, length.out=100),
                         #main="2008 to 2022",
                         xlab=NULL,
                         ylab=NULL,
                         colorkey=F)+
  latticeExtra::layer({
    grid.text(x= 5.9, y=54.8, "SSP1-2.6\noptimistic\n2085",
              gp=gpar(fontsize=8), rot=0,
              default.units='native',
              just="left")
  })
D1

E1<-rasterVis::levelplot(raster_late_ssp126_2085_median$var1.pred_total,margin=F,
                         #col.regions=turbo(100),
                         col.regions=viridis(100, direction = -1),
                         at=seq(0, 100, length.out=100),
                         #main="2008 to 2022",
                         xlab=NULL,
                         ylab=NULL,
                         colorkey=F)+
  latticeExtra::layer({
    grid.text(x= 5.9, y=54.8, "SSP1-2.6\naverage\n2085",
              gp=gpar(fontsize=8), rot=0,
              default.units='native',
              just="left")
  })
E1

F1<-rasterVis::levelplot(raster_late_ssp126_2085_quantile90$var1.pred_total,margin=F,
                         #col.regions=turbo(100),
                         col.regions=viridis(100, direction = -1),
                         at=seq(0, 100, length.out=100),
                         #main="2008 to 2022",
                         #scales=list(y=list(draw=FALSE)),
                         ylab=NULL,
                         xlab=NULL,
                         colorkey=list(title=list("Probability\nof frost\ndamage\n>50 %\n[%]",fontsize=8))) +
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
    grid.text(x= 12.5, y=47.05, "kilometers",
              gp=gpar(cex=0.5), rot=0,
              default.units='native',
              just="left")
  })+
  latticeExtra::layer({
    grid.text(x= 5.9, y=54.8, "SSP1-2.6\npessimistic\n2085",
              gp=gpar(fontsize=8), rot=0,
              default.units='native',
              just="left")
  })

F1

# Combination via `c.trellis`
ssp126_late_all <- c(A1,B1,C1,D1,E1,F1, layout = c(3,2), merge.legends = T)
print(ssp126_late_all)
update(ssp126_late_all, scales = list(alternating = 1))

png("new_names_ssp126_late_damage50.png", pointsize=8, width=5700, height=4600, res=600)
ssp126_late_all <- c(A1,B1,C1,D1,E1,F1, layout = c(3,2), merge.legends = T)
print(ssp126_late_all)
update(ssp126_late_all, scales = list(alternating = 1))
dev.off()


#ssp 245 late####
raster_late_ssp245_2050_median<-readRDS("data/map_preparation/map_data_late_ssp245.csv_damage_50_2050_median.rds")
raster_late_ssp245_2050_quantile10<-readRDS("data/map_preparation/map_data_late_ssp245.csv_damage_50_2050_quantile10.rds")
raster_late_ssp245_2050_quantile90<-readRDS("data/map_preparation/map_data_late_ssp245.csv_damage_50_2050_quantile90.rds")
raster_late_ssp245_2085_median<-readRDS("data/map_preparation/map_data_late_ssp245.csv_damage_50_2085_median.rds")
raster_late_ssp245_2085_quantile10<-readRDS("data/map_preparation/map_data_late_ssp245.csv_damage_50_2085_quantile10.rds")
raster_late_ssp245_2085_quantile90<-readRDS("data/map_preparation/map_data_late_ssp245.csv_damage_50_2085_quantile90.rds")


raster_late_ssp245_2050_median<-mask(raster_late_ssp245_2050_median, DE_Bundeslaender_ohne_Meer_EPSG_4326_poly)
raster_late_ssp245_2050_quantile10<-mask(raster_late_ssp245_2050_quantile10, DE_Bundeslaender_ohne_Meer_EPSG_4326_poly)
raster_late_ssp245_2050_quantile90<-mask(raster_late_ssp245_2050_quantile90, DE_Bundeslaender_ohne_Meer_EPSG_4326_poly)
raster_late_ssp245_2085_median<-mask(raster_late_ssp245_2085_median, DE_Bundeslaender_ohne_Meer_EPSG_4326_poly)
raster_late_ssp245_2085_quantile10<-mask(raster_late_ssp245_2085_quantile10, DE_Bundeslaender_ohne_Meer_EPSG_4326_poly)
raster_late_ssp245_2085_quantile90<-mask(raster_late_ssp245_2085_quantile90, DE_Bundeslaender_ohne_Meer_EPSG_4326_poly)



A1<-rasterVis::levelplot(raster_late_ssp245_2050_quantile10$var1.pred_total,
                         margin=F,
                         #col.regions=turbo(100),
                         col.regions=viridis(100, direction = -1),
                         at=seq(0, 100, length.out=100),
                         #main="quantile 10",
                         colorkey=F,
                         #scales=list(x=list(draw=FALSE)),
                         xlab="Longitude",
                         ylab="Latitude")+
  latticeExtra::layer({
    grid.text(x= 5.9, y=54.8, "SSP2-4.5\noptimistic\n2050",
              gp=gpar(fontsize=8), rot=0,
              default.units='native',
              just="left")
  })
A1


B1<-rasterVis::levelplot(raster_late_ssp245_2050_median$var1.pred_total,margin=F,
                         #col.regions=turbo(100),
                         col.regions=viridis(100, direction = -1),
                         at=seq(0, 100, length.out=100),
                         #main="2008 to 2022",
                         xlab=NULL,
                         ylab=NULL,
                         colorkey=F)+
  latticeExtra::layer({
    grid.text(x= 5.9, y=54.8, "SSP2-4.5\naverage\n2050",
              gp=gpar(fontsize=8), rot=0,
              default.units='native',
              just="left")
  })
B1

C1<-rasterVis::levelplot(raster_late_ssp245_2050_quantile90$var1.pred_total,margin=F,
                         #col.regions=turbo(100),
                         col.regions=viridis(100, direction = -1),
                         at=seq(0, 100, length.out=100),
                         #main="2008 to 2022",
                         xlab=NULL,
                         ylab=NULL,
                         colorkey=F)+
  latticeExtra::layer({
    grid.text(x= 5.9, y=54.8, "SSP2-4.5\npessimistic\n2050",
              gp=gpar(fontsize=8), rot=0,
              default.units='native',
              just="left")
  })
C1

D1<-rasterVis::levelplot(raster_late_ssp245_2085_quantile10$var1.pred_total,margin=F,
                         #col.regions=turbo(100),
                         col.regions=viridis(100, direction = -1),
                         at=seq(0, 100, length.out=100),
                         #main="2008 to 2022",
                         xlab=NULL,
                         ylab=NULL,
                         colorkey=F)+
  latticeExtra::layer({
    grid.text(x= 5.9, y=54.8, "SSP2-4.5\noptimistic\n2085",
              gp=gpar(fontsize=8), rot=0,
              default.units='native',
              just="left")
  })
D1

E1<-rasterVis::levelplot(raster_late_ssp245_2085_median$var1.pred_total,margin=F,
                         #col.regions=turbo(100),
                         col.regions=viridis(100, direction = -1),
                         at=seq(0, 100, length.out=100),
                         #main="2008 to 2022",
                         xlab=NULL,
                         ylab=NULL,
                         colorkey=F)+
  latticeExtra::layer({
    grid.text(x= 5.9, y=54.8, "SSP2-4.5\naverage\n2085",
              gp=gpar(fontsize=8), rot=0,
              default.units='native',
              just="left")
  })
E1

F1<-rasterVis::levelplot(raster_late_ssp245_2085_quantile90$var1.pred_total,margin=F,
                         #col.regions=turbo(100),
                         col.regions=viridis(100, direction = -1),
                         at=seq(0, 100, length.out=100),
                         #main="2008 to 2022",
                         #scales=list(y=list(draw=FALSE)),
                         ylab=NULL,
                         xlab=NULL,
                         colorkey=list(title=list("Probability\nof frost\ndamage\n>50 %\n[%]",fontsize=8))) +
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
    grid.text(x= 12.5, y=47.05, "kilometers",
              gp=gpar(cex=0.5), rot=0,
              default.units='native',
              just="left")
  })+
  latticeExtra::layer({
    grid.text(x= 5.9, y=54.8, "SSP2-4.5\npessimistic\n2085",
              gp=gpar(fontsize=8), rot=0,
              default.units='native',
              just="left")
  })

F1

# Combination via `c.trellis`
ssp245_late_all <- c(A1,B1,C1,D1,E1,F1, layout = c(3,2), merge.legends = T)
print(ssp245_late_all)
update(ssp245_late_all, scales = list(alternating = 1))

png("new_names_ssp245_late_damage50.png", pointsize=8, width=5700, height=4600, res=600)
ssp245_late_all <- c(A1,B1,C1,D1,E1,F1, layout = c(3,2), merge.legends = T)
print(ssp245_late_all)
update(ssp245_late_all, scales = list(alternating = 1))
dev.off()

#ssp 370 late####
raster_late_ssp370_2050_median<-readRDS("data/map_preparation/map_data_late_ssp370.csv_damage_50_2050_median.rds")
raster_late_ssp370_2050_quantile10<-readRDS("data/map_preparation/map_data_late_ssp370.csv_damage_50_2050_quantile10.rds")
raster_late_ssp370_2050_quantile90<-readRDS("data/map_preparation/map_data_late_ssp370.csv_damage_50_2050_quantile90.rds")
raster_late_ssp370_2085_median<-readRDS("data/map_preparation/map_data_late_ssp370.csv_damage_50_2085_median.rds")
raster_late_ssp370_2085_quantile10<-readRDS("data/map_preparation/map_data_late_ssp370.csv_damage_50_2085_quantile10.rds")
raster_late_ssp370_2085_quantile90<-readRDS("data/map_preparation/map_data_late_ssp370.csv_damage_50_2085_quantile90.rds")


raster_late_ssp370_2050_median<-mask(raster_late_ssp370_2050_median, DE_Bundeslaender_ohne_Meer_EPSG_4326_poly)
raster_late_ssp370_2050_quantile10<-mask(raster_late_ssp370_2050_quantile10, DE_Bundeslaender_ohne_Meer_EPSG_4326_poly)
raster_late_ssp370_2050_quantile90<-mask(raster_late_ssp370_2050_quantile90, DE_Bundeslaender_ohne_Meer_EPSG_4326_poly)
raster_late_ssp370_2085_median<-mask(raster_late_ssp370_2085_median, DE_Bundeslaender_ohne_Meer_EPSG_4326_poly)
raster_late_ssp370_2085_quantile10<-mask(raster_late_ssp370_2085_quantile10, DE_Bundeslaender_ohne_Meer_EPSG_4326_poly)
raster_late_ssp370_2085_quantile90<-mask(raster_late_ssp370_2085_quantile90, DE_Bundeslaender_ohne_Meer_EPSG_4326_poly)



A1<-rasterVis::levelplot(raster_late_ssp370_2050_quantile10$var1.pred_total,
                         margin=F,
                         #col.regions=turbo(100),
                         col.regions=viridis(100, direction = -1),
                         at=seq(0, 100, length.out=100),
                         #main="quantile 10",
                         colorkey=F,
                         #scales=list(x=list(draw=FALSE)),
                         xlab="Longitude",
                         ylab="Latitude")+
  latticeExtra::layer({
    grid.text(x= 5.9, y=54.8, "SSP3-7.0\noptimistic\n2050",
              gp=gpar(fontsize=8), rot=0,
              default.units='native',
              just="left")
  })
A1


B1<-rasterVis::levelplot(raster_late_ssp370_2050_median$var1.pred_total,margin=F,
                         #col.regions=turbo(100),
                         col.regions=viridis(100, direction = -1),
                         at=seq(0, 100, length.out=100),
                         #main="2008 to 2022",
                         xlab=NULL,
                         ylab=NULL,
                         colorkey=F)+
  latticeExtra::layer({
    grid.text(x= 5.9, y=54.8, "SSP3-7.0\naverage\n2050",
              gp=gpar(fontsize=8), rot=0,
              default.units='native',
              just="left")
  })
B1

C1<-rasterVis::levelplot(raster_late_ssp370_2050_quantile90$var1.pred_total,margin=F,
                         #col.regions=turbo(100),
                         col.regions=viridis(100, direction = -1),
                         at=seq(0, 100, length.out=100),
                         #main="2008 to 2022",
                         xlab=NULL,
                         ylab=NULL,
                         colorkey=F)+
  latticeExtra::layer({
    grid.text(x= 5.9, y=54.8, "SSP3-7.0\npessimistic\n2050",
              gp=gpar(fontsize=8), rot=0,
              default.units='native',
              just="left")
  })
C1

D1<-rasterVis::levelplot(raster_late_ssp370_2085_quantile10$var1.pred_total,margin=F,
                         #col.regions=turbo(100),
                         col.regions=viridis(100, direction = -1),
                         at=seq(0, 100, length.out=100),
                         #main="2008 to 2022",
                         xlab=NULL,
                         ylab=NULL,
                         colorkey=F)+
  latticeExtra::layer({
    grid.text(x= 5.9, y=54.8, "SSP3-7.0\noptimistic\n2085",
              gp=gpar(fontsize=8), rot=0,
              default.units='native',
              just="left")
  })
D1

E1<-rasterVis::levelplot(raster_late_ssp370_2085_median$var1.pred_total,margin=F,
                         #col.regions=turbo(100),
                         col.regions=viridis(100, direction = -1),
                         at=seq(0, 100, length.out=100),
                         #main="2008 to 2022",
                         xlab=NULL,
                         ylab=NULL,
                         colorkey=F)+
  latticeExtra::layer({
    grid.text(x= 5.9, y=54.8, "SSP3-7.0\naverage\n2085",
              gp=gpar(fontsize=8), rot=0,
              default.units='native',
              just="left")
  })
E1

F1<-rasterVis::levelplot(raster_late_ssp370_2085_quantile90$var1.pred_total,margin=F,
                         #col.regions=turbo(100),
                         col.regions=viridis(100, direction = -1),
                         at=seq(0, 100, length.out=100),
                         #main="2008 to 2022",
                         #scales=list(y=list(draw=FALSE)),
                         ylab=NULL,
                         xlab=NULL,
                         colorkey=list(title=list("Probability\nof frost\ndamage\n>50 %\n[%]",fontsize=8))) +
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
    grid.text(x= 12.5, y=47.05, "kilometers",
              gp=gpar(cex=0.5), rot=0,
              default.units='native',
              just="left")
  })+
  latticeExtra::layer({
    grid.text(x= 5.9, y=54.8, "SSP3-7.0\npessimistic\n2085",
              gp=gpar(fontsize=8), rot=0,
              default.units='native',
              just="left")
  })

F1

# Combination via `c.trellis`
ssp370_late_all <- c(A1,B1,C1,D1,E1,F1, layout = c(3,2), merge.legends = T)
print(ssp370_late_all)
update(ssp370_late_all, scales = list(alternating = 1))

png("new_names_ssp370_late_damage50.png", pointsize=8, width=5700, height=4600, res=600)
ssp370_late_all <- c(A1,B1,C1,D1,E1,F1, layout = c(3,2), merge.legends = T)
print(ssp370_late_all)
update(ssp370_late_all, scales = list(alternating = 1))
dev.off()


