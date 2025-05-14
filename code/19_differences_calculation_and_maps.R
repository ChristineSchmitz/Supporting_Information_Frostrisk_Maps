#Load raster data and libraries with script 11 and 12

#differences between 1993-2022 and 2050, 1993-2022 and 2085 as well as 2050 and 2085####
###damage >10%####
diff_raster_early_ssp245_2050_2085<-raster_early_ssp245_2050_median$var1.pred_total - raster_early_ssp245_2085_median$var1.pred_total
diff_raster_early_ssp126_2050_2085<-raster_early_ssp126_2050_median$var1.pred_total - raster_early_ssp126_2085_median$var1.pred_total
diff_raster_early_ssp370_2050_2085<-raster_early_ssp370_2050_median$var1.pred_total - raster_early_ssp370_2085_median$var1.pred_total
diff_raster_early_ssp585_2050_2085<-raster_early_ssp585_2050_median$var1.pred_total - raster_early_ssp585_2085_median$var1.pred_total

diff_raster_early_ssp245_hist_2050<-raster_hist_early$var1.pred_total - raster_early_ssp245_2050_median$var1.pred_total
diff_raster_early_ssp126_hist_2050<-raster_hist_early$var1.pred_total - raster_early_ssp126_2050_median$var1.pred_total
diff_raster_early_ssp370_hist_2050<-raster_hist_early$var1.pred_total - raster_early_ssp370_2050_median$var1.pred_total
diff_raster_early_ssp585_hist_2050<-raster_hist_early$var1.pred_total - raster_early_ssp585_2050_median$var1.pred_total

diff_raster_early_ssp245_hist_2085<-raster_hist_early$var1.pred_total - raster_early_ssp245_2085_median$var1.pred_total
diff_raster_early_ssp126_hist_2085<-raster_hist_early$var1.pred_total - raster_early_ssp126_2085_median$var1.pred_total
diff_raster_early_ssp370_hist_2085<-raster_hist_early$var1.pred_total - raster_early_ssp370_2085_median$var1.pred_total
diff_raster_early_ssp585_hist_2085<-raster_hist_early$var1.pred_total - raster_early_ssp585_2085_median$var1.pred_total

diff_df_early_ssp126_2050_2085<-as.data.frame(diff_raster_early_ssp126_2050_2085, xy=T)
diff_df_early_ssp245_2050_2085<-as.data.frame(diff_raster_early_ssp245_2050_2085, xy=T)
diff_df_early_ssp370_2050_2085<-as.data.frame(diff_raster_early_ssp370_2050_2085, xy=T)
diff_df_early_ssp585_2050_2085<-as.data.frame(diff_raster_early_ssp585_2050_2085, xy=T)

diff_df_early_ssp126_hist_2050<-as.data.frame(diff_raster_early_ssp126_hist_2050, xy=T)
diff_df_early_ssp245_hist_2050<-as.data.frame(diff_raster_early_ssp245_hist_2050, xy=T)
diff_df_early_ssp370_hist_2050<-as.data.frame(diff_raster_early_ssp370_hist_2050, xy=T)
diff_df_early_ssp585_hist_2050<-as.data.frame(diff_raster_early_ssp585_hist_2050, xy=T)

diff_df_early_ssp126_hist_2085<-as.data.frame(diff_raster_early_ssp126_hist_2085, xy=T)
diff_df_early_ssp245_hist_2085<-as.data.frame(diff_raster_early_ssp245_hist_2085, xy=T)
diff_df_early_ssp370_hist_2085<-as.data.frame(diff_raster_early_ssp370_hist_2085, xy=T)
diff_df_early_ssp585_hist_2085<-as.data.frame(diff_raster_early_ssp585_hist_2085, xy=T)

#summarize differences ###
diff_df_early_ssp245_2050_2085_summary<-diff_df_early_ssp245_2050_2085%>%
  summarise(n_increase=sum(layer<0, na.rm = T),
            n_decrease=sum(layer>0, na.rm = T),
            n_unchanged=sum(layer==0, na.rm = T),
            perc_increase=sum(layer<0, na.rm = T)/length(na.omit(layer)),
            perc_decrease=sum(layer>0, na.rm = T)/length(na.omit(layer)),
            perc_unchangede=sum(layer==0, na.rm = T)/length(na.omit(layer)),
            min= min(layer, na.rm = T),
            max= max(layer, na.rm = T),
            median= median(layer, na.rm = T),
            mean = mean(layer, na.rm = T))
diff_df_early_ssp245_2050_2085_summary$pathway<-"ssp245"
diff_df_early_ssp245_2050_2085_summary$model<-"median"
diff_df_early_ssp245_2050_2085_summary$comparison<-"2050_2085"

diff_df_early_ssp126_2050_2085_summary<-diff_df_early_ssp126_2050_2085%>%
  summarise(n_increase=sum(layer<0, na.rm = T),
            n_decrease=sum(layer>0, na.rm = T),
            n_unchanged=sum(layer==0, na.rm = T),
            perc_increase=sum(layer<0, na.rm = T)/length(na.omit(layer)),
            perc_decrease=sum(layer>0, na.rm = T)/length(na.omit(layer)),
            perc_unchangede=sum(layer==0, na.rm = T)/length(na.omit(layer)),
            min= min(layer, na.rm = T),
            max= max(layer, na.rm = T),
            median= median(layer, na.rm = T),
            mean = mean(layer, na.rm = T))
diff_df_early_ssp126_2050_2085_summary$pathway<-"ssp126"
diff_df_early_ssp126_2050_2085_summary$model<-"median"
diff_df_early_ssp126_2050_2085_summary$comparison<-"2050_2085"

diff_df_early_ssp370_2050_2085_summary<-diff_df_early_ssp370_2050_2085%>%
  summarise(n_increase=sum(layer<0, na.rm = T),
            n_decrease=sum(layer>0, na.rm = T),
            n_unchanged=sum(layer==0, na.rm = T),
            perc_increase=sum(layer<0, na.rm = T)/length(na.omit(layer)),
            perc_decrease=sum(layer>0, na.rm = T)/length(na.omit(layer)),
            perc_unchangede=sum(layer==0, na.rm = T)/length(na.omit(layer)),
            min= min(layer, na.rm = T),
            max= max(layer, na.rm = T),
            median= median(layer, na.rm = T),
            mean = mean(layer, na.rm = T))
diff_df_early_ssp370_2050_2085_summary$pathway<-"ssp370"
diff_df_early_ssp370_2050_2085_summary$model<-"median"
diff_df_early_ssp370_2050_2085_summary$comparison<-"2050_2085"

diff_df_early_ssp585_2050_2085_summary<-diff_df_early_ssp585_2050_2085%>%
  summarise(n_increase=sum(layer<0, na.rm = T),
            n_decrease=sum(layer>0, na.rm = T),
            n_unchanged=sum(layer==0, na.rm = T),
            perc_increase=sum(layer<0, na.rm = T)/length(na.omit(layer)),
            perc_decrease=sum(layer>0, na.rm = T)/length(na.omit(layer)),
            perc_unchangede=sum(layer==0, na.rm = T)/length(na.omit(layer)),
            min= min(layer, na.rm = T),
            max= max(layer, na.rm = T),
            median= median(layer, na.rm = T),
            mean = mean(layer, na.rm = T))
diff_df_early_ssp585_2050_2085_summary$pathway<-"ssp585"
diff_df_early_ssp585_2050_2085_summary$model<-"median"
diff_df_early_ssp585_2050_2085_summary$comparison<-"2050_2085"

diff_df_early_ssp245_hist_2050_summary<-diff_df_early_ssp245_hist_2050%>%
  summarise(n_increase=sum(layer<0, na.rm = T),
            n_decrease=sum(layer>0, na.rm = T),
            n_unchanged=sum(layer==0, na.rm = T),
            perc_increase=sum(layer<0, na.rm = T)/length(na.omit(layer)),
            perc_decrease=sum(layer>0, na.rm = T)/length(na.omit(layer)),
            perc_unchangede=sum(layer==0, na.rm = T)/length(na.omit(layer)),
            min= min(layer, na.rm = T),
            max= max(layer, na.rm = T),
            median= median(layer, na.rm = T),
            mean = mean(layer, na.rm = T))
diff_df_early_ssp245_hist_2050_summary$pathway<-"ssp245"
diff_df_early_ssp245_hist_2050_summary$model<-"median"
diff_df_early_ssp245_hist_2050_summary$comparison<-"hist_2050"

diff_df_early_ssp126_hist_2050_summary<-diff_df_early_ssp126_hist_2050%>%
  summarise(n_increase=sum(layer<0, na.rm = T),
            n_decrease=sum(layer>0, na.rm = T),
            n_unchanged=sum(layer==0, na.rm = T),
            perc_increase=sum(layer<0, na.rm = T)/length(na.omit(layer)),
            perc_decrease=sum(layer>0, na.rm = T)/length(na.omit(layer)),
            perc_unchangede=sum(layer==0, na.rm = T)/length(na.omit(layer)),
            min= min(layer, na.rm = T),
            max= max(layer, na.rm = T),
            median= median(layer, na.rm = T),
            mean = mean(layer, na.rm = T))
diff_df_early_ssp126_hist_2050_summary$pathway<-"ssp126"
diff_df_early_ssp126_hist_2050_summary$model<-"median"
diff_df_early_ssp126_hist_2050_summary$comparison<-"hist_2050"

diff_df_early_ssp370_hist_2050_summary<-diff_df_early_ssp370_hist_2050%>%
  summarise(n_increase=sum(layer<0, na.rm = T),
            n_decrease=sum(layer>0, na.rm = T),
            n_unchanged=sum(layer==0, na.rm = T),
            perc_increase=sum(layer<0, na.rm = T)/length(na.omit(layer)),
            perc_decrease=sum(layer>0, na.rm = T)/length(na.omit(layer)),
            perc_unchangede=sum(layer==0, na.rm = T)/length(na.omit(layer)),
            min= min(layer, na.rm = T),
            max= max(layer, na.rm = T),
            median= median(layer, na.rm = T),
            mean = mean(layer, na.rm = T))
diff_df_early_ssp370_hist_2050_summary$pathway<-"ssp370"
diff_df_early_ssp370_hist_2050_summary$model<-"median"
diff_df_early_ssp370_hist_2050_summary$comparison<-"hist_2050"

diff_df_early_ssp585_hist_2050_summary<-diff_df_early_ssp585_hist_2050%>%
  summarise(n_increase=sum(layer<0, na.rm = T),
            n_decrease=sum(layer>0, na.rm = T),
            n_unchanged=sum(layer==0, na.rm = T),
            perc_increase=sum(layer<0, na.rm = T)/length(na.omit(layer)),
            perc_decrease=sum(layer>0, na.rm = T)/length(na.omit(layer)),
            perc_unchangede=sum(layer==0, na.rm = T)/length(na.omit(layer)),
            min= min(layer, na.rm = T),
            max= max(layer, na.rm = T),
            median= median(layer, na.rm = T),
            mean = mean(layer, na.rm = T))
diff_df_early_ssp585_hist_2050_summary$pathway<-"ssp585"
diff_df_early_ssp585_hist_2050_summary$model<-"median"
diff_df_early_ssp585_hist_2050_summary$comparison<-"hist_2050"


diff_df_early_ssp245_hist_2085_summary<-diff_df_early_ssp245_hist_2085%>%
  summarise(n_increase=sum(layer<0, na.rm = T),
            n_decrease=sum(layer>0, na.rm = T),
            n_unchanged=sum(layer==0, na.rm = T),
            perc_increase=sum(layer<0, na.rm = T)/length(na.omit(layer)),
            perc_decrease=sum(layer>0, na.rm = T)/length(na.omit(layer)),
            perc_unchangede=sum(layer==0, na.rm = T)/length(na.omit(layer)),
            min= min(layer, na.rm = T),
            max= max(layer, na.rm = T),
            median= median(layer, na.rm = T),
            mean = mean(layer, na.rm = T))
diff_df_early_ssp245_hist_2085_summary$pathway<-"ssp245"
diff_df_early_ssp245_hist_2085_summary$model<-"median"
diff_df_early_ssp245_hist_2085_summary$comparison<-"hist_2085"

diff_df_early_ssp126_hist_2085_summary<-diff_df_early_ssp126_hist_2085%>%
  summarise(n_increase=sum(layer<0, na.rm = T),
            n_decrease=sum(layer>0, na.rm = T),
            n_unchanged=sum(layer==0, na.rm = T),
            perc_increase=sum(layer<0, na.rm = T)/length(na.omit(layer)),
            perc_decrease=sum(layer>0, na.rm = T)/length(na.omit(layer)),
            perc_unchangede=sum(layer==0, na.rm = T)/length(na.omit(layer)),
            min= min(layer, na.rm = T),
            max= max(layer, na.rm = T),
            median= median(layer, na.rm = T),
            mean = mean(layer, na.rm = T))
diff_df_early_ssp126_hist_2085_summary$pathway<-"ssp126"
diff_df_early_ssp126_hist_2085_summary$model<-"median"
diff_df_early_ssp126_hist_2085_summary$comparison<-"hist_2085"

diff_df_early_ssp370_hist_2085_summary<-diff_df_early_ssp370_hist_2085%>%
  summarise(n_increase=sum(layer<0, na.rm = T),
            n_decrease=sum(layer>0, na.rm = T),
            n_unchanged=sum(layer==0, na.rm = T),
            perc_increase=sum(layer<0, na.rm = T)/length(na.omit(layer)),
            perc_decrease=sum(layer>0, na.rm = T)/length(na.omit(layer)),
            perc_unchangede=sum(layer==0, na.rm = T)/length(na.omit(layer)),
            min= min(layer, na.rm = T),
            max= max(layer, na.rm = T),
            median= median(layer, na.rm = T),
            mean = mean(layer, na.rm = T))
diff_df_early_ssp370_hist_2085_summary$pathway<-"ssp370"
diff_df_early_ssp370_hist_2085_summary$model<-"median"
diff_df_early_ssp370_hist_2085_summary$comparison<-"hist_2085"

diff_df_early_ssp585_hist_2085_summary<-diff_df_early_ssp585_hist_2085%>%
  summarise(n_increase=sum(layer<0, na.rm = T),
            n_decrease=sum(layer>0, na.rm = T),
            n_unchanged=sum(layer==0, na.rm = T),
            perc_increase=sum(layer<0, na.rm = T)/length(na.omit(layer)),
            perc_decrease=sum(layer>0, na.rm = T)/length(na.omit(layer)),
            perc_unchangede=sum(layer==0, na.rm = T)/length(na.omit(layer)),
            min= min(layer, na.rm = T),
            max= max(layer, na.rm = T),
            median= median(layer, na.rm = T),
            mean = mean(layer, na.rm = T))
diff_df_early_ssp585_hist_2085_summary$pathway<-"ssp585"
diff_df_early_ssp585_hist_2085_summary$model<-"median"
diff_df_early_ssp585_hist_2085_summary$comparison<-"hist_2085"

summary_early_years<-rbind(diff_df_early_ssp126_hist_2050_summary,
                           diff_df_early_ssp245_hist_2050_summary,
                           diff_df_early_ssp370_hist_2050_summary,
                           diff_df_early_ssp585_hist_2050_summary,
                           diff_df_early_ssp126_2050_2085_summary,
                           diff_df_early_ssp245_2050_2085_summary,
                           diff_df_early_ssp370_2050_2085_summary,
                           diff_df_early_ssp585_2050_2085_summary,
                           diff_df_early_ssp126_hist_2085_summary,
                           diff_df_early_ssp245_hist_2085_summary,
                           diff_df_early_ssp370_hist_2085_summary,
                           diff_df_early_ssp585_hist_2085_summary)



byr <- colorRampPalette(colors = c("magenta3", "beige", "green4"))

#plot differences###
diff_plot_ssp126_2050_2085<-rasterVis::levelplot(diff_raster_early_ssp126_2050_2085$layer,
                         margin=F,
                         col.regions=byr(120),
                         #col.regions=viridis(100, direction = -1),
                         #at=shift_seq,
                         at=seq(-60, 60, length.out=120),
                         #main="quantile 10",
                         colorkey=F,
                         #scales=list(x=list(draw=FALSE)),
                         xlab="Longitude",
                         ylab="Latitude")+
  latticeExtra::layer({
    grid.text(x= 5.9, y=54.8, "SSP1-2.6",
              gp=gpar(fontsize=8), rot=0,
              default.units='native',
              just="left")
  })
diff_plot_ssp126_2050_2085

diff_plot_ssp126_hist_2050<-rasterVis::levelplot(diff_raster_early_ssp126_hist_2050$layer,
                                                 margin=F,
                                                 col.regions=byr(120),
                                                 #col.regions=viridis(100, direction = -1),
                                                 at=seq(-60, 60, length.out=120),
                                                 #main="quantile 10",
                                                 colorkey=F,
                                                 #scales=list(x=list(draw=FALSE)),
                                                 xlab="Longitude",
                                                 ylab="Latitude")+
  latticeExtra::layer({
    grid.text(x= 5.9, y=54.8, "SSP1-2.6",
              gp=gpar(fontsize=8), rot=0,
              default.units='native',
              just="left")
  })
diff_plot_ssp126_hist_2050

diff_plot_ssp126_hist_2085<-rasterVis::levelplot(diff_raster_early_ssp126_hist_2085$layer,
                                                 margin=F,
                                                 col.regions=byr(120),
                                                 #col.regions=viridis(100, direction = -1),
                                                 at=seq(-60, 60, length.out=120),
                                                 #main="quantile 10",
                                                 colorkey=F,
                                                 #scales=list(x=list(draw=FALSE)),
                                                 xlab="Longitude",
                                                 ylab="Latitude")+
  latticeExtra::layer({
    grid.text(x= 5.9, y=54.8, "SSP1-2.6",
              gp=gpar(fontsize=8), rot=0,
              default.units='native',
              just="left")
  })
diff_plot_ssp126_hist_2085

diff_plot_ssp245_2050_2085<-rasterVis::levelplot(diff_raster_early_ssp245_2050_2085$layer,
                                                 margin=F,
                                                 col.regions=byr(120),
                                                 #col.regions=viridis(100, direction = -1),
                                                 at=seq(-60, 60, length.out=120),
                                                 #main="quantile 10",
                                                 colorkey=F,
                                                 #scales=list(x=list(draw=FALSE)),
                                                 xlab="Longitude",
                                                 ylab="Latitude")+
  latticeExtra::layer({
    grid.text(x= 5.9, y=54.8, "SSP2-4.5",
              gp=gpar(fontsize=8), rot=0,
              default.units='native',
              just="left")
  })
diff_plot_ssp245_2050_2085

diff_plot_ssp245_hist_2050<-rasterVis::levelplot(diff_raster_early_ssp245_hist_2050$layer,
                                                 margin=F,
                                                 col.regions=byr(120),
                                                 #col.regions=viridis(100, direction = -1),
                                                 at=seq(-60, 60, length.out=120),
                                                 #main="quantile 10",
                                                 colorkey=F,
                                                 #scales=list(x=list(draw=FALSE)),
                                                 xlab="Longitude",
                                                 ylab="Latitude")+
  latticeExtra::layer({
    grid.text(x= 5.9, y=54.8, "SSP2-4.5",
              gp=gpar(fontsize=8), rot=0,
              default.units='native',
              just="left")
  })
diff_plot_ssp245_hist_2050

diff_plot_ssp245_hist_2085<-rasterVis::levelplot(diff_raster_early_ssp245_hist_2085$layer,
                                                 margin=F,
                                                 col.regions=byr(120),
                                                 #col.regions=viridis(100, direction = -1),
                                                 at=seq(-60, 60, length.out=120),
                                                 #main="quantile 10",
                                                 colorkey=F,
                                                 #scales=list(x=list(draw=FALSE)),
                                                 xlab="Longitude",
                                                 ylab="Latitude")+
  latticeExtra::layer({
    grid.text(x= 5.9, y=54.8, "SSP2-4.5",
              gp=gpar(fontsize=8), rot=0,
              default.units='native',
              just="left")
  })
diff_plot_ssp245_hist_2085

diff_plot_ssp370_2050_2085<-rasterVis::levelplot(diff_raster_early_ssp370_2050_2085$layer,
                                                 margin=F,
                                                 col.regions=byr(120),
                                                 #col.regions=viridis(100, direction = -1),
                                                 at=seq(-60, 60, length.out=120),
                                                 #main="quantile 10",
                                                 colorkey=F,
                                                 #scales=list(x=list(draw=FALSE)),
                                                 xlab="Longitude",
                                                 ylab="Latitude")+
  latticeExtra::layer({
    grid.text(x= 5.9, y=54.8, "SSP3-7.0",
              gp=gpar(fontsize=8), rot=0,
              default.units='native',
              just="left")
  })
diff_plot_ssp370_2050_2085

diff_plot_ssp370_hist_2050<-rasterVis::levelplot(diff_raster_early_ssp370_hist_2050$layer,
                                                 margin=F,
                                                 col.regions=byr(120),
                                                 #col.regions=viridis(100, direction = -1),
                                                 at=seq(-60, 60, length.out=120),
                                                 #main="quantile 10",
                                                 colorkey=F,
                                                 #scales=list(x=list(draw=FALSE)),
                                                 xlab="Longitude",
                                                 ylab="Latitude")+
  latticeExtra::layer({
    grid.text(x= 5.9, y=54.8, "SSP3-7.0",
              gp=gpar(fontsize=8), rot=0,
              default.units='native',
              just="left")
  })
diff_plot_ssp370_hist_2050

diff_plot_ssp370_hist_2085<-rasterVis::levelplot(diff_raster_early_ssp370_hist_2085$layer,
                                                 margin=F,
                                                 col.regions=byr(120),
                                                 #col.regions=viridis(100, direction = -1),
                                                 at=seq(-60, 60, length.out=120),
                                                 #main="quantile 10",
                                                 colorkey=F,
                                                 #scales=list(x=list(draw=FALSE)),
                                                 xlab="Longitude",
                                                 ylab="Latitude")+
  latticeExtra::layer({
    grid.text(x= 5.9, y=54.8, "SSP3-7.0",
              gp=gpar(fontsize=8), rot=0,
              default.units='native',
              just="left")
  })
diff_plot_ssp370_hist_2085

diff_plot_ssp585_2050_2085<-rasterVis::levelplot(diff_raster_early_ssp585_2050_2085$layer,
                                                 margin=F,
                                                 col.regions=byr(120),
                                                 #col.regions=viridis(100, direction = -1),
                                                 at=seq(-60, 60, length.out=120),
                                                 #main="quantile 10",
                                                 #colorkey=F,
                                                 #scales=list(x=list(draw=FALSE)),
                                                 xlab="Longitude",
                                                 ylab="Latitude",
                                                 colorkey=list(title=list("Change\nof the\nprobability\n of frost\ndamage\n>10 %\n[Percentage\npoints]",fontsize=8))) +
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
diff_plot_ssp585_2050_2085

diff_plot_ssp585_hist_2050<-rasterVis::levelplot(diff_raster_early_ssp585_hist_2050$layer,
                                                 margin=F,
                                                 col.regions=byr(120),
                                                 #col.regions=viridis(100, direction = -1),
                                                 at=seq(-60, 60, length.out=120),
                                                 #main="quantile 10",
                                                 colorkey=F,
                                                 #scales=list(x=list(draw=FALSE)),
                                                 xlab="Longitude",
                                                 ylab="Latitude")+
  latticeExtra::layer({
    grid.text(x= 5.9, y=54.8, "SSP5-8.5",
              gp=gpar(fontsize=8), rot=0,
              default.units='native',
              just="left")
  })
diff_plot_ssp585_hist_2050

diff_plot_ssp585_hist_2085<-rasterVis::levelplot(diff_raster_early_ssp585_hist_2085$layer,
                                                 margin=F,
                                                 col.regions=byr(120),
                                                 #col.regions=viridis(100, direction = -1),
                                                 at=seq(-60, 60, length.out=120),
                                                 #main="quantile 10",
                                                 colorkey=F,
                                                 #scales=list(x=list(draw=FALSE)),
                                                 xlab="Longitude",
                                                 ylab="Latitude")+
  latticeExtra::layer({
    grid.text(x= 5.9, y=54.8, "SSP5-8.5",
              gp=gpar(fontsize=8), rot=0,
              default.units='native',
              just="left")
  })
diff_plot_ssp585_hist_2085


diff_plot_ssp585_hist_2085_legend<-rasterVis::levelplot(diff_raster_early_ssp585_hist_2085$layer,
                                                        margin=F,
                                                        col.regions=byr(120),
                                                        #col.regions=viridis(100, direction = -1),
                                                        at=seq(-60, 60, length.out=120),
                                                        #main="quantile 10",
                                                        #colorkey=F,
                                                        #scales=list(x=list(draw=FALSE)),
                                                        xlab="Longitude",
                                                        ylab="Latitude",
                                                        colorkey=list(title=list("Change\nof the\nprobability\n of frost\ndamage\n>10 %\n[Percentage\npoints]",fontsize=8))) +
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
diff_plot_ssp585_hist_2085_legend


library(grid)

png("differnce_plot_damage10_early_new.png", pointsize=8, width=3600, height=8000, res=600)
differnce_plot_damage10_early<-c(diff_plot_ssp126_hist_2050,
                                 diff_plot_ssp126_hist_2085,
                                 diff_plot_ssp245_hist_2050,
                                 diff_plot_ssp245_hist_2085,
                                 diff_plot_ssp370_hist_2050,
                                 diff_plot_ssp370_hist_2085,
                                 diff_plot_ssp585_hist_2050,
                                 diff_plot_ssp585_hist_2085_legend,
                                 layout=c(2,4), merge.legends=T)
print(differnce_plot_damage10_early)

#print(median_all_ssp, position = c(0, 0, 1, 1), more = TRUE)
update(differnce_plot_damage10_early, scales = list(alternating = 1))
# Add column titles manually
grid.text(label = c("difference between\n1993-2022 and 2050", "difference between\n1993-2022 and 2085"), 
          x = seq(0.325, 0.7, length.out = 2),  # Adjust positions for column headers
          y = 0.98, gp = gpar(fontsize = 11, fontface = "bold"))
dev.off()

png("differnce_plot_damage10_early2_with_hist-2085.png", pointsize=8, width=4900, height=8000, res=600)
differnce_plot_damage10_early<-c(diff_plot_ssp126_hist_2050,
                                 diff_plot_ssp126_hist_2085,
                                 diff_plot_ssp126_2050_2085,
                                 diff_plot_ssp245_hist_2050,
                                 diff_plot_ssp245_hist_2085,
                                 diff_plot_ssp245_2050_2085,
                                 diff_plot_ssp370_hist_2050,
                                 diff_plot_ssp370_hist_2085,
                                 diff_plot_ssp370_2050_2085,
                                 diff_plot_ssp585_hist_2050,
                                 diff_plot_ssp585_hist_2085,
                                 diff_plot_ssp585_2050_2085,
                                 layout=c(3,4), merge.legends=T)
print(differnce_plot_damage10_early)

#print(median_all_ssp, position = c(0, 0, 1, 1), more = TRUE)
update(differnce_plot_damage10_early, scales = list(alternating = 1))
# Add column titles manually
grid.text(label = c("difference between\n1993-2022 and 2050","difference between\n1993-2022 and 2085", "difference between\n2050 and 2085"), 
          #x = seq(0.25, 0.75, length.out = 3),  # Adjust positions for column headers
          x = c(0.25, 0.52, 0.78), 
          y = 0.98, gp = gpar(fontsize = 11, fontface = "bold"))
dev.off()
##damage >50%####
#Load raster data and libraries with script 11 and 12

diff_raster_early_ssp245_2050_2085<-raster_early_ssp245_2050_median$var1.pred_total - raster_early_ssp245_2085_median$var1.pred_total
diff_raster_early_ssp126_2050_2085<-raster_early_ssp126_2050_median$var1.pred_total - raster_early_ssp126_2085_median$var1.pred_total
diff_raster_early_ssp370_2050_2085<-raster_early_ssp370_2050_median$var1.pred_total - raster_early_ssp370_2085_median$var1.pred_total
diff_raster_early_ssp585_2050_2085<-raster_early_ssp585_2050_median$var1.pred_total - raster_early_ssp585_2085_median$var1.pred_total

diff_raster_early_ssp245_hist_2050<-raster_hist_early$var1.pred_total - raster_early_ssp245_2050_median$var1.pred_total
diff_raster_early_ssp126_hist_2050<-raster_hist_early$var1.pred_total - raster_early_ssp126_2050_median$var1.pred_total
diff_raster_early_ssp370_hist_2050<-raster_hist_early$var1.pred_total - raster_early_ssp370_2050_median$var1.pred_total
diff_raster_early_ssp585_hist_2050<-raster_hist_early$var1.pred_total - raster_early_ssp585_2050_median$var1.pred_total

diff_raster_early_ssp245_hist_2085<-raster_hist_early$var1.pred_total - raster_early_ssp245_2085_median$var1.pred_total
diff_raster_early_ssp126_hist_2085<-raster_hist_early$var1.pred_total - raster_early_ssp126_2085_median$var1.pred_total
diff_raster_early_ssp370_hist_2085<-raster_hist_early$var1.pred_total - raster_early_ssp370_2085_median$var1.pred_total
diff_raster_early_ssp585_hist_2085<-raster_hist_early$var1.pred_total - raster_early_ssp585_2085_median$var1.pred_total

diff_df_early_ssp126_2050_2085<-as.data.frame(diff_raster_early_ssp126_2050_2085, xy=T)
diff_df_early_ssp245_2050_2085<-as.data.frame(diff_raster_early_ssp245_2050_2085, xy=T)
diff_df_early_ssp370_2050_2085<-as.data.frame(diff_raster_early_ssp370_2050_2085, xy=T)
diff_df_early_ssp585_2050_2085<-as.data.frame(diff_raster_early_ssp585_2050_2085, xy=T)

diff_df_early_ssp126_hist_2050<-as.data.frame(diff_raster_early_ssp126_hist_2050, xy=T)
diff_df_early_ssp245_hist_2050<-as.data.frame(diff_raster_early_ssp245_hist_2050, xy=T)
diff_df_early_ssp370_hist_2050<-as.data.frame(diff_raster_early_ssp370_hist_2050, xy=T)
diff_df_early_ssp585_hist_2050<-as.data.frame(diff_raster_early_ssp585_hist_2050, xy=T)

diff_df_early_ssp126_hist_2085<-as.data.frame(diff_raster_early_ssp126_hist_2085, xy=T)
diff_df_early_ssp245_hist_2085<-as.data.frame(diff_raster_early_ssp245_hist_2085, xy=T)
diff_df_early_ssp370_hist_2085<-as.data.frame(diff_raster_early_ssp370_hist_2085, xy=T)
diff_df_early_ssp585_hist_2085<-as.data.frame(diff_raster_early_ssp585_hist_2085, xy=T)

#summarize differences ###

diff_df_early_ssp245_2050_2085_summary<-diff_df_early_ssp245_2050_2085%>%
  summarise(n_increase=sum(layer<0, na.rm = T),
            n_decrease=sum(layer>0, na.rm = T),
            n_unchanged=sum(layer==0, na.rm = T),
            perc_increase=sum(layer<0, na.rm = T)/length(na.omit(layer)),
            perc_decrease=sum(layer>0, na.rm = T)/length(na.omit(layer)),
            perc_unchangede=sum(layer==0, na.rm = T)/length(na.omit(layer)),
            min= min(layer, na.rm = T),
            max= max(layer, na.rm = T),
            median= median(layer, na.rm = T),
            mean = mean(layer, na.rm = T))
diff_df_early_ssp245_2050_2085_summary$pathway<-"ssp245"
diff_df_early_ssp245_2050_2085_summary$model<-"median"
diff_df_early_ssp245_2050_2085_summary$comparison<-"2050_2085"

diff_df_early_ssp126_2050_2085_summary<-diff_df_early_ssp126_2050_2085%>%
  summarise(n_increase=sum(layer<0, na.rm = T),
            n_decrease=sum(layer>0, na.rm = T),
            n_unchanged=sum(layer==0, na.rm = T),
            perc_increase=sum(layer<0, na.rm = T)/length(na.omit(layer)),
            perc_decrease=sum(layer>0, na.rm = T)/length(na.omit(layer)),
            perc_unchangede=sum(layer==0, na.rm = T)/length(na.omit(layer)),
            min= min(layer, na.rm = T),
            max= max(layer, na.rm = T),
            median= median(layer, na.rm = T),
            mean = mean(layer, na.rm = T))
diff_df_early_ssp126_2050_2085_summary$pathway<-"ssp126"
diff_df_early_ssp126_2050_2085_summary$model<-"median"
diff_df_early_ssp126_2050_2085_summary$comparison<-"2050_2085"

diff_df_early_ssp370_2050_2085_summary<-diff_df_early_ssp370_2050_2085%>%
  summarise(n_increase=sum(layer<0, na.rm = T),
            n_decrease=sum(layer>0, na.rm = T),
            n_unchanged=sum(layer==0, na.rm = T),
            perc_increase=sum(layer<0, na.rm = T)/length(na.omit(layer)),
            perc_decrease=sum(layer>0, na.rm = T)/length(na.omit(layer)),
            perc_unchangede=sum(layer==0, na.rm = T)/length(na.omit(layer)),
            min= min(layer, na.rm = T),
            max= max(layer, na.rm = T),
            median= median(layer, na.rm = T),
            mean = mean(layer, na.rm = T))
diff_df_early_ssp370_2050_2085_summary$pathway<-"ssp370"
diff_df_early_ssp370_2050_2085_summary$model<-"median"
diff_df_early_ssp370_2050_2085_summary$comparison<-"2050_2085"

diff_df_early_ssp585_2050_2085_summary<-diff_df_early_ssp585_2050_2085%>%
  summarise(n_increase=sum(layer<0, na.rm = T),
            n_decrease=sum(layer>0, na.rm = T),
            n_unchanged=sum(layer==0, na.rm = T),
            perc_increase=sum(layer<0, na.rm = T)/length(na.omit(layer)),
            perc_decrease=sum(layer>0, na.rm = T)/length(na.omit(layer)),
            perc_unchangede=sum(layer==0, na.rm = T)/length(na.omit(layer)),
            min= min(layer, na.rm = T),
            max= max(layer, na.rm = T),
            median= median(layer, na.rm = T),
            mean = mean(layer, na.rm = T))
diff_df_early_ssp585_2050_2085_summary$pathway<-"ssp585"
diff_df_early_ssp585_2050_2085_summary$model<-"median"
diff_df_early_ssp585_2050_2085_summary$comparison<-"2050_2085"

diff_df_early_ssp245_hist_2050_summary<-diff_df_early_ssp245_hist_2050%>%
  summarise(n_increase=sum(layer<0, na.rm = T),
            n_decrease=sum(layer>0, na.rm = T),
            n_unchanged=sum(layer==0, na.rm = T),
            perc_increase=sum(layer<0, na.rm = T)/length(na.omit(layer)),
            perc_decrease=sum(layer>0, na.rm = T)/length(na.omit(layer)),
            perc_unchangede=sum(layer==0, na.rm = T)/length(na.omit(layer)),
            min= min(layer, na.rm = T),
            max= max(layer, na.rm = T),
            median= median(layer, na.rm = T),
            mean = mean(layer, na.rm = T))
diff_df_early_ssp245_hist_2050_summary$pathway<-"ssp245"
diff_df_early_ssp245_hist_2050_summary$model<-"median"
diff_df_early_ssp245_hist_2050_summary$comparison<-"hist_2050"

diff_df_early_ssp126_hist_2050_summary<-diff_df_early_ssp126_hist_2050%>%
  summarise(n_increase=sum(layer<0, na.rm = T),
            n_decrease=sum(layer>0, na.rm = T),
            n_unchanged=sum(layer==0, na.rm = T),
            perc_increase=sum(layer<0, na.rm = T)/length(na.omit(layer)),
            perc_decrease=sum(layer>0, na.rm = T)/length(na.omit(layer)),
            perc_unchangede=sum(layer==0, na.rm = T)/length(na.omit(layer)),
            min= min(layer, na.rm = T),
            max= max(layer, na.rm = T),
            median= median(layer, na.rm = T),
            mean = mean(layer, na.rm = T))
diff_df_early_ssp126_hist_2050_summary$pathway<-"ssp126"
diff_df_early_ssp126_hist_2050_summary$model<-"median"
diff_df_early_ssp126_hist_2050_summary$comparison<-"hist_2050"

diff_df_early_ssp370_hist_2050_summary<-diff_df_early_ssp370_hist_2050%>%
  summarise(n_increase=sum(layer<0, na.rm = T),
            n_decrease=sum(layer>0, na.rm = T),
            n_unchanged=sum(layer==0, na.rm = T),
            perc_increase=sum(layer<0, na.rm = T)/length(na.omit(layer)),
            perc_decrease=sum(layer>0, na.rm = T)/length(na.omit(layer)),
            perc_unchangede=sum(layer==0, na.rm = T)/length(na.omit(layer)),
            min= min(layer, na.rm = T),
            max= max(layer, na.rm = T),
            median= median(layer, na.rm = T),
            mean = mean(layer, na.rm = T))
diff_df_early_ssp370_hist_2050_summary$pathway<-"ssp370"
diff_df_early_ssp370_hist_2050_summary$model<-"median"
diff_df_early_ssp370_hist_2050_summary$comparison<-"hist_2050"

diff_df_early_ssp585_hist_2050_summary<-diff_df_early_ssp585_hist_2050%>%
  summarise(n_increase=sum(layer<0, na.rm = T),
            n_decrease=sum(layer>0, na.rm = T),
            n_unchanged=sum(layer==0, na.rm = T),
            perc_increase=sum(layer<0, na.rm = T)/length(na.omit(layer)),
            perc_decrease=sum(layer>0, na.rm = T)/length(na.omit(layer)),
            perc_unchangede=sum(layer==0, na.rm = T)/length(na.omit(layer)),
            min= min(layer, na.rm = T),
            max= max(layer, na.rm = T),
            median= median(layer, na.rm = T),
            mean = mean(layer, na.rm = T))
diff_df_early_ssp585_hist_2050_summary$pathway<-"ssp585"
diff_df_early_ssp585_hist_2050_summary$model<-"median"
diff_df_early_ssp585_hist_2050_summary$comparison<-"hist_2050"


diff_df_early_ssp245_hist_2085_summary<-diff_df_early_ssp245_hist_2085%>%
  summarise(n_increase=sum(layer<0, na.rm = T),
            n_decrease=sum(layer>0, na.rm = T),
            n_unchanged=sum(layer==0, na.rm = T),
            perc_increase=sum(layer<0, na.rm = T)/length(na.omit(layer)),
            perc_decrease=sum(layer>0, na.rm = T)/length(na.omit(layer)),
            perc_unchangede=sum(layer==0, na.rm = T)/length(na.omit(layer)),
            min= min(layer, na.rm = T),
            max= max(layer, na.rm = T),
            median= median(layer, na.rm = T),
            mean = mean(layer, na.rm = T))
diff_df_early_ssp245_hist_2085_summary$pathway<-"ssp245"
diff_df_early_ssp245_hist_2085_summary$model<-"median"
diff_df_early_ssp245_hist_2085_summary$comparison<-"hist_2085"

diff_df_early_ssp126_hist_2085_summary<-diff_df_early_ssp126_hist_2085%>%
  summarise(n_increase=sum(layer<0, na.rm = T),
            n_decrease=sum(layer>0, na.rm = T),
            n_unchanged=sum(layer==0, na.rm = T),
            perc_increase=sum(layer<0, na.rm = T)/length(na.omit(layer)),
            perc_decrease=sum(layer>0, na.rm = T)/length(na.omit(layer)),
            perc_unchangede=sum(layer==0, na.rm = T)/length(na.omit(layer)),
            min= min(layer, na.rm = T),
            max= max(layer, na.rm = T),
            median= median(layer, na.rm = T),
            mean = mean(layer, na.rm = T))
diff_df_early_ssp126_hist_2085_summary$pathway<-"ssp126"
diff_df_early_ssp126_hist_2085_summary$model<-"median"
diff_df_early_ssp126_hist_2085_summary$comparison<-"hist_2085"

diff_df_early_ssp370_hist_2085_summary<-diff_df_early_ssp370_hist_2085%>%
  summarise(n_increase=sum(layer<0, na.rm = T),
            n_decrease=sum(layer>0, na.rm = T),
            n_unchanged=sum(layer==0, na.rm = T),
            perc_increase=sum(layer<0, na.rm = T)/length(na.omit(layer)),
            perc_decrease=sum(layer>0, na.rm = T)/length(na.omit(layer)),
            perc_unchangede=sum(layer==0, na.rm = T)/length(na.omit(layer)),
            min= min(layer, na.rm = T),
            max= max(layer, na.rm = T),
            median= median(layer, na.rm = T),
            mean = mean(layer, na.rm = T))
diff_df_early_ssp370_hist_2085_summary$pathway<-"ssp370"
diff_df_early_ssp370_hist_2085_summary$model<-"median"
diff_df_early_ssp370_hist_2085_summary$comparison<-"hist_2085"

diff_df_early_ssp585_hist_2085_summary<-diff_df_early_ssp585_hist_2085%>%
  summarise(n_increase=sum(layer<0, na.rm = T),
            n_decrease=sum(layer>0, na.rm = T),
            n_unchanged=sum(layer==0, na.rm = T),
            perc_increase=sum(layer<0, na.rm = T)/length(na.omit(layer)),
            perc_decrease=sum(layer>0, na.rm = T)/length(na.omit(layer)),
            perc_unchangede=sum(layer==0, na.rm = T)/length(na.omit(layer)),
            min= min(layer, na.rm = T),
            max= max(layer, na.rm = T),
            median= median(layer, na.rm = T),
            mean = mean(layer, na.rm = T))
diff_df_early_ssp585_hist_2085_summary$pathway<-"ssp585"
diff_df_early_ssp585_hist_2085_summary$model<-"median"
diff_df_early_ssp585_hist_2085_summary$comparison<-"hist_2085"

summary_early_years<-rbind(diff_df_early_ssp126_hist_2050_summary,
                           diff_df_early_ssp245_hist_2050_summary,
                           diff_df_early_ssp370_hist_2050_summary,
                           diff_df_early_ssp585_hist_2050_summary,
                           diff_df_early_ssp126_2050_2085_summary,
                           diff_df_early_ssp245_2050_2085_summary,
                           diff_df_early_ssp370_2050_2085_summary,
                           diff_df_early_ssp585_2050_2085_summary,
                           diff_df_early_ssp126_hist_2085_summary,
                           diff_df_early_ssp245_hist_2085_summary,
                           diff_df_early_ssp370_hist_2085_summary,
                           diff_df_early_ssp585_hist_2085_summary)



byr <- colorRampPalette(colors = c("magenta3", "beige", "green4"))

#plot differences
diff_plot_ssp126_2050_2085<-rasterVis::levelplot(diff_raster_early_ssp126_2050_2085$layer,
                                                 margin=F,
                                                 col.regions=byr(120),
                                                 #col.regions=viridis(100, direction = -1),
                                                 #at=shift_seq,
                                                 at=seq(-60, 60, length.out=120),
                                                 #main="quantile 10",
                                                 colorkey=F,
                                                 #scales=list(x=list(draw=FALSE)),
                                                 xlab="Longitude",
                                                 ylab="Latitude")+
  latticeExtra::layer({
    grid.text(x= 5.9, y=54.8, "SSP1-2.6",
              gp=gpar(fontsize=8), rot=0,
              default.units='native',
              just="left")
  })
diff_plot_ssp126_2050_2085

diff_plot_ssp126_hist_2050<-rasterVis::levelplot(diff_raster_early_ssp126_hist_2050$layer,
                                                 margin=F,
                                                 col.regions=byr(120),
                                                 #col.regions=viridis(100, direction = -1),
                                                 at=seq(-60, 60, length.out=120),
                                                 #main="quantile 10",
                                                 colorkey=F,
                                                 #scales=list(x=list(draw=FALSE)),
                                                 xlab="Longitude",
                                                 ylab="Latitude")+
  latticeExtra::layer({
    grid.text(x= 5.9, y=54.8, "SSP1-2.6",
              gp=gpar(fontsize=8), rot=0,
              default.units='native',
              just="left")
  })
diff_plot_ssp126_hist_2050

diff_plot_ssp126_hist_2085<-rasterVis::levelplot(diff_raster_early_ssp126_hist_2085$layer,
                                                 margin=F,
                                                 col.regions=byr(120),
                                                 #col.regions=viridis(100, direction = -1),
                                                 at=seq(-60, 60, length.out=120),
                                                 #main="quantile 10",
                                                 colorkey=F,
                                                 #scales=list(x=list(draw=FALSE)),
                                                 xlab="Longitude",
                                                 ylab="Latitude")+
  latticeExtra::layer({
    grid.text(x= 5.9, y=54.8, "SSP1-2.6",
              gp=gpar(fontsize=8), rot=0,
              default.units='native',
              just="left")
  })
diff_plot_ssp126_hist_2085


diff_plot_ssp245_2050_2085<-rasterVis::levelplot(diff_raster_early_ssp245_2050_2085$layer,
                                                 margin=F,
                                                 col.regions=byr(120),
                                                 #col.regions=viridis(100, direction = -1),
                                                 at=seq(-60, 60, length.out=120),
                                                 #main="quantile 10",
                                                 colorkey=F,
                                                 #scales=list(x=list(draw=FALSE)),
                                                 xlab="Longitude",
                                                 ylab="Latitude")+
  latticeExtra::layer({
    grid.text(x= 5.9, y=54.8, "SSP2-4.5",
              gp=gpar(fontsize=8), rot=0,
              default.units='native',
              just="left")
  })
diff_plot_ssp245_2050_2085

diff_plot_ssp245_hist_2050<-rasterVis::levelplot(diff_raster_early_ssp245_hist_2050$layer,
                                                 margin=F,
                                                 col.regions=byr(120),
                                                 #col.regions=viridis(100, direction = -1),
                                                 at=seq(-60, 60, length.out=120),
                                                 #main="quantile 10",
                                                 colorkey=F,
                                                 #scales=list(x=list(draw=FALSE)),
                                                 xlab="Longitude",
                                                 ylab="Latitude")+
  latticeExtra::layer({
    grid.text(x= 5.9, y=54.8, "SSP2-4.5",
              gp=gpar(fontsize=8), rot=0,
              default.units='native',
              just="left")
  })
diff_plot_ssp245_hist_2050

diff_plot_ssp245_hist_2085<-rasterVis::levelplot(diff_raster_early_ssp245_hist_2085$layer,
                                                 margin=F,
                                                 col.regions=byr(120),
                                                 #col.regions=viridis(100, direction = -1),
                                                 at=seq(-60, 60, length.out=120),
                                                 #main="quantile 10",
                                                 colorkey=F,
                                                 #scales=list(x=list(draw=FALSE)),
                                                 xlab="Longitude",
                                                 ylab="Latitude")+
  latticeExtra::layer({
    grid.text(x= 5.9, y=54.8, "SSP2-4.5",
              gp=gpar(fontsize=8), rot=0,
              default.units='native',
              just="left")
  })
diff_plot_ssp245_hist_2085


diff_plot_ssp370_2050_2085<-rasterVis::levelplot(diff_raster_early_ssp370_2050_2085$layer,
                                                 margin=F,
                                                 col.regions=byr(120),
                                                 #col.regions=viridis(100, direction = -1),
                                                 at=seq(-60, 60, length.out=120),
                                                 #main="quantile 10",
                                                 colorkey=F,
                                                 #scales=list(x=list(draw=FALSE)),
                                                 xlab="Longitude",
                                                 ylab="Latitude")+
  latticeExtra::layer({
    grid.text(x= 5.9, y=54.8, "SSP3-7.0",
              gp=gpar(fontsize=8), rot=0,
              default.units='native',
              just="left")
  })
diff_plot_ssp370_2050_2085

diff_plot_ssp370_hist_2050<-rasterVis::levelplot(diff_raster_early_ssp370_hist_2050$layer,
                                                 margin=F,
                                                 col.regions=byr(120),
                                                 #col.regions=viridis(100, direction = -1),
                                                 at=seq(-60, 60, length.out=120),
                                                 #main="quantile 10",
                                                 colorkey=F,
                                                 #scales=list(x=list(draw=FALSE)),
                                                 xlab="Longitude",
                                                 ylab="Latitude")+
  latticeExtra::layer({
    grid.text(x= 5.9, y=54.8, "SSP3-7.0",
              gp=gpar(fontsize=8), rot=0,
              default.units='native',
              just="left")
  })
diff_plot_ssp370_hist_2050

diff_plot_ssp370_hist_2085<-rasterVis::levelplot(diff_raster_early_ssp370_hist_2085$layer,
                                                 margin=F,
                                                 col.regions=byr(120),
                                                 #col.regions=viridis(100, direction = -1),
                                                 at=seq(-60, 60, length.out=120),
                                                 #main="quantile 10",
                                                 colorkey=F,
                                                 #scales=list(x=list(draw=FALSE)),
                                                 xlab="Longitude",
                                                 ylab="Latitude")+
  latticeExtra::layer({
    grid.text(x= 5.9, y=54.8, "SSP3-7.0",
              gp=gpar(fontsize=8), rot=0,
              default.units='native',
              just="left")
  })
diff_plot_ssp370_hist_2085


diff_plot_ssp585_2050_2085<-rasterVis::levelplot(diff_raster_early_ssp585_2050_2085$layer,
                                                 margin=F,
                                                 col.regions=byr(120),
                                                 #col.regions=viridis(100, direction = -1),
                                                 at=seq(-60, 60, length.out=120),
                                                 #main="quantile 10",
                                                 #colorkey=F,
                                                 #scales=list(x=list(draw=FALSE)),
                                                 xlab="Longitude",
                                                 ylab="Latitude",
                                                 colorkey=list(title=list("Change\nof the\nprobability\n of frost\ndamage\n>50 %\n[Percentage\npoints]",fontsize=8))) +
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
diff_plot_ssp585_2050_2085

diff_plot_ssp585_hist_2050<-rasterVis::levelplot(diff_raster_early_ssp585_hist_2050$layer,
                                                 margin=F,
                                                 col.regions=byr(120),
                                                 #col.regions=viridis(100, direction = -1),
                                                 at=seq(-60, 60, length.out=120),
                                                 #main="quantile 10",
                                                 colorkey=F,
                                                 #scales=list(x=list(draw=FALSE)),
                                                 xlab="Longitude",
                                                 ylab="Latitude")+
  latticeExtra::layer({
    grid.text(x= 5.9, y=54.8, "SSP5-8.5",
              gp=gpar(fontsize=8), rot=0,
              default.units='native',
              just="left")
  })
diff_plot_ssp585_hist_2050

diff_plot_ssp585_hist_2085<-rasterVis::levelplot(diff_raster_early_ssp585_hist_2085$layer,
                                                 margin=F,
                                                 col.regions=byr(120),
                                                 #col.regions=viridis(100, direction = -1),
                                                 at=seq(-60, 60, length.out=120),
                                                 #main="quantile 10",
                                                 colorkey=F,
                                                 #scales=list(x=list(draw=FALSE)),
                                                 xlab="Longitude",
                                                 ylab="Latitude")+
  latticeExtra::layer({
    grid.text(x= 5.9, y=54.8, "SSP5-8.5",
              gp=gpar(fontsize=8), rot=0,
              default.units='native',
              just="left")
  })
diff_plot_ssp585_hist_2085


diff_plot_ssp585_hist_2085_legend<-rasterVis::levelplot(diff_raster_early_ssp585_hist_2085$layer,
                                                        margin=F,
                                                        col.regions=byr(120),
                                                        #col.regions=viridis(100, direction = -1),
                                                        at=seq(-60, 60, length.out=120),
                                                        #main="quantile 10",
                                                        #colorkey=F,
                                                        #scales=list(x=list(draw=FALSE)),
                                                        xlab="Longitude",
                                                        ylab="Latitude",
                                                        colorkey=list(title=list("Change\nof the\nprobability\n of frost\ndamage\n>50 %\n[Percentage\npoints]",fontsize=8))) +
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
diff_plot_ssp585_hist_2085_legend


library(grid)

png("differnce_plot_damage50_early_new.png", pointsize=8, width=3600, height=8000, res=600)
differnce_plot_damage10_early<-c(diff_plot_ssp126_hist_2050,
                                 diff_plot_ssp126_hist_2085,
                                 diff_plot_ssp245_hist_2050,
                                 diff_plot_ssp245_hist_2085,
                                 diff_plot_ssp370_hist_2050,
                                 diff_plot_ssp370_hist_2085,
                                 diff_plot_ssp585_hist_2050,
                                 diff_plot_ssp585_hist_2085_legend,
                                 layout=c(2,4), merge.legends=T)
print(differnce_plot_damage10_early)

#print(median_all_ssp, position = c(0, 0, 1, 1), more = TRUE)
update(differnce_plot_damage10_early, scales = list(alternating = 1))
# Add column titles manually
grid.text(label = c("difference between\n1993-2022 and 2050", "difference between\n1993-2022 and 2085"), 
          x = seq(0.325, 0.7, length.out = 2),  # Adjust positions for column headers
          y = 0.98, gp = gpar(fontsize = 11, fontface = "bold"))
dev.off()

png("differnce_plot_damage50_early2_with_hist-2085.png", pointsize=8, width=4900, height=8000, res=600)
differnce_plot_damage50_early<-c(diff_plot_ssp126_hist_2050,
                                 diff_plot_ssp126_hist_2085,
                                 diff_plot_ssp126_2050_2085,
                                 diff_plot_ssp245_hist_2050,
                                 diff_plot_ssp245_hist_2085,
                                 diff_plot_ssp245_2050_2085,
                                 diff_plot_ssp370_hist_2050,
                                 diff_plot_ssp370_hist_2085,
                                 diff_plot_ssp370_2050_2085,
                                 diff_plot_ssp585_hist_2050,
                                 diff_plot_ssp585_hist_2085,
                                 diff_plot_ssp585_2050_2085,
                                 layout=c(3,4), merge.legends=T)
print(differnce_plot_damage50_early)

#print(median_all_ssp, position = c(0, 0, 1, 1), more = TRUE)
update(differnce_plot_damage50_early, scales = list(alternating = 1))
# Add column titles manually
grid.text(label = c("difference between\n1993-2022 and 2050","difference between\n1993-2022 and 2085", "difference between\n2050 and 2085"), 
          #x = seq(0.25, 0.75, length.out = 3),  # Adjust positions for column headers
          x = c(0.25, 0.52, 0.78), 
          y = 0.98, gp = gpar(fontsize = 11, fontface = "bold"))
dev.off()

#differences between early & late####
diff_raster_ssp126_2050_early_late<-raster_early_ssp126_2050_median$var1.pred_total - raster_late_ssp126_2050_median$var1.pred_total
diff_raster_ssp245_2050_early_late<-raster_early_ssp245_2050_median$var1.pred_total - raster_late_ssp245_2050_median$var1.pred_total
diff_raster_ssp370_2050_early_late<-raster_early_ssp370_2050_median$var1.pred_total - raster_late_ssp370_2050_median$var1.pred_total
diff_raster_ssp585_2050_early_late<-raster_early_ssp585_2050_median$var1.pred_total - raster_late_ssp585_2050_median$var1.pred_total

diff_raster_ssp126_2085_early_late<-raster_early_ssp126_2085_median$var1.pred_total - raster_late_ssp126_2085_median$var1.pred_total
diff_raster_ssp245_2085_early_late<-raster_early_ssp245_2085_median$var1.pred_total - raster_late_ssp245_2085_median$var1.pred_total
diff_raster_ssp370_2085_early_late<-raster_early_ssp370_2085_median$var1.pred_total - raster_late_ssp370_2085_median$var1.pred_total
diff_raster_ssp585_2085_early_late<-raster_early_ssp585_2085_median$var1.pred_total - raster_late_ssp585_2085_median$var1.pred_total

diff_df_ssp126_2050_early_late<-as.data.frame(diff_raster_ssp126_2050_early_late, xy=T)
diff_df_ssp245_2050_early_late<-as.data.frame(diff_raster_ssp245_2050_early_late, xy=T)
diff_df_ssp370_2050_early_late<-as.data.frame(diff_raster_ssp370_2050_early_late, xy=T)
diff_df_ssp585_2050_early_late<-as.data.frame(diff_raster_ssp585_2050_early_late, xy=T)

diff_df_ssp126_2085_early_late<-as.data.frame(diff_raster_ssp126_2085_early_late, xy=T)
diff_df_ssp245_2085_early_late<-as.data.frame(diff_raster_ssp245_2085_early_late, xy=T)
diff_df_ssp370_2085_early_late<-as.data.frame(diff_raster_ssp370_2085_early_late, xy=T)
diff_df_ssp585_2085_early_late<-as.data.frame(diff_raster_ssp585_2085_early_late, xy=T)

#summarize differences##
diff_df_ssp126_2050_early_late_summary<-diff_df_ssp126_2050_early_late%>%
  summarise(n_increase=sum(layer<0, na.rm = T),
            n_decrease=sum(layer>0, na.rm = T),
            n_unchanged=sum(layer==0, na.rm = T),
            perc_increase=sum(layer<0, na.rm = T)/length(na.omit(layer)),
            perc_decrease=sum(layer>0, na.rm = T)/length(na.omit(layer)),
            perc_unchangede=sum(layer==0, na.rm = T)/length(na.omit(layer)),
            min= min(layer, na.rm = T),
            max= max(layer, na.rm = T),
            median= median(layer, na.rm = T),
            q25=quantile(layer,0.25, na.rm=T),
            q50=quantile(layer,0.5, na.rm=T),
            mean = mean(layer, na.rm = T))
diff_df_ssp126_2050_early_late_summary$pathway<-"ssp126"
diff_df_ssp126_2050_early_late_summary$model<-"median"
diff_df_ssp126_2050_early_late_summary$comparison<-"early_late"

diff_df_ssp245_2050_early_late_summary<-diff_df_ssp245_2050_early_late%>%
  summarise(n_increase=sum(layer<0, na.rm = T),
            n_decrease=sum(layer>0, na.rm = T),
            n_unchanged=sum(layer==0, na.rm = T),
            perc_increase=sum(layer<0, na.rm = T)/length(na.omit(layer)),
            perc_decrease=sum(layer>0, na.rm = T)/length(na.omit(layer)),
            perc_unchangede=sum(layer==0, na.rm = T)/length(na.omit(layer)),
            min= min(layer, na.rm = T),
            max= max(layer, na.rm = T),
            median= median(layer, na.rm = T),
            q25=quantile(layer,0.25, na.rm=T),
            q50=quantile(layer,0.5, na.rm=T),
            mean = mean(layer, na.rm = T))
diff_df_ssp245_2050_early_late_summary$pathway<-"ssp245"
diff_df_ssp245_2050_early_late_summary$model<-"median"
diff_df_ssp245_2050_early_late_summary$comparison<-"early_late"

diff_df_ssp370_2050_early_late_summary<-diff_df_ssp370_2050_early_late%>%
  summarise(n_increase=sum(layer<0, na.rm = T),
            n_decrease=sum(layer>0, na.rm = T),
            n_unchanged=sum(layer==0, na.rm = T),
            perc_increase=sum(layer<0, na.rm = T)/length(na.omit(layer)),
            perc_decrease=sum(layer>0, na.rm = T)/length(na.omit(layer)),
            perc_unchangede=sum(layer==0, na.rm = T)/length(na.omit(layer)),
            min= min(layer, na.rm = T),
            max= max(layer, na.rm = T),
            median= median(layer, na.rm = T),
            q25=quantile(layer,0.25, na.rm=T),
            q50=quantile(layer,0.5, na.rm=T),
            mean = mean(layer, na.rm = T))
diff_df_ssp370_2050_early_late_summary$pathway<-"ssp370"
diff_df_ssp370_2050_early_late_summary$model<-"median"
diff_df_ssp370_2050_early_late_summary$comparison<-"early_late"

diff_df_ssp585_2050_early_late_summary<-diff_df_ssp585_2050_early_late%>%
  summarise(n_increase=sum(layer<0, na.rm = T),
            n_decrease=sum(layer>0, na.rm = T),
            n_unchanged=sum(layer==0, na.rm = T),
            perc_increase=sum(layer<0, na.rm = T)/length(na.omit(layer)),
            perc_decrease=sum(layer>0, na.rm = T)/length(na.omit(layer)),
            perc_unchangede=sum(layer==0, na.rm = T)/length(na.omit(layer)),
            min= min(layer, na.rm = T),
            max= max(layer, na.rm = T),
            median= median(layer, na.rm = T),
            q25=quantile(layer,0.25, na.rm=T),
            q50=quantile(layer,0.5, na.rm=T),
            mean = mean(layer, na.rm = T))
diff_df_ssp585_2050_early_late_summary$pathway<-"ssp585"
diff_df_ssp585_2050_early_late_summary$model<-"median"
diff_df_ssp585_2050_early_late_summary$comparison<-"early_late"

diff_df_ssp126_2085_early_late_summary<-diff_df_ssp126_2085_early_late%>%
  summarise(n_increase=sum(layer<0, na.rm = T),
            n_decrease=sum(layer>0, na.rm = T),
            n_unchanged=sum(layer==0, na.rm = T),
            perc_increase=sum(layer<0, na.rm = T)/length(na.omit(layer)),
            perc_decrease=sum(layer>0, na.rm = T)/length(na.omit(layer)),
            perc_unchangede=sum(layer==0, na.rm = T)/length(na.omit(layer)),
            min= min(layer, na.rm = T),
            max= max(layer, na.rm = T),
            median= median(layer, na.rm = T),
            q25=quantile(layer,0.25, na.rm=T),
            q50=quantile(layer,0.5, na.rm=T),
            mean = mean(layer, na.rm = T))
diff_df_ssp126_2085_early_late_summary$pathway<-"ssp126"
diff_df_ssp126_2085_early_late_summary$model<-"median"
diff_df_ssp126_2085_early_late_summary$comparison<-"early_late"

diff_df_ssp245_2085_early_late_summary<-diff_df_ssp245_2085_early_late%>%
  summarise(n_increase=sum(layer<0, na.rm = T),
            n_decrease=sum(layer>0, na.rm = T),
            n_unchanged=sum(layer==0, na.rm = T),
            perc_increase=sum(layer<0, na.rm = T)/length(na.omit(layer)),
            perc_decrease=sum(layer>0, na.rm = T)/length(na.omit(layer)),
            perc_unchangede=sum(layer==0, na.rm = T)/length(na.omit(layer)),
            min= min(layer, na.rm = T),
            max= max(layer, na.rm = T),
            median= median(layer, na.rm = T),
            q25=quantile(layer,0.25, na.rm=T),
            q50=quantile(layer,0.5, na.rm=T),
            mean = mean(layer, na.rm = T))
diff_df_ssp245_2085_early_late_summary$pathway<-"ssp245"
diff_df_ssp245_2085_early_late_summary$model<-"median"
diff_df_ssp245_2085_early_late_summary$comparison<-"early_late"

diff_df_ssp370_2085_early_late_summary<-diff_df_ssp370_2085_early_late%>%
  summarise(n_increase=sum(layer<0, na.rm = T),
            n_decrease=sum(layer>0, na.rm = T),
            n_unchanged=sum(layer==0, na.rm = T),
            perc_increase=sum(layer<0, na.rm = T)/length(na.omit(layer)),
            perc_decrease=sum(layer>0, na.rm = T)/length(na.omit(layer)),
            perc_unchangede=sum(layer==0, na.rm = T)/length(na.omit(layer)),
            min= min(layer, na.rm = T),
            max= max(layer, na.rm = T),
            median= median(layer, na.rm = T),
            q25=quantile(layer,0.25, na.rm=T),
            q50=quantile(layer,0.5, na.rm=T),
            mean = mean(layer, na.rm = T))
diff_df_ssp370_2085_early_late_summary$pathway<-"ssp370"
diff_df_ssp370_2085_early_late_summary$model<-"median"
diff_df_ssp370_2085_early_late_summary$comparison<-"early_late"

diff_df_ssp585_2085_early_late_summary<-diff_df_ssp585_2085_early_late%>%
  summarise(n_increase=sum(layer<0, na.rm = T),
            n_decrease=sum(layer>0, na.rm = T),
            n_unchanged=sum(layer==0, na.rm = T),
            perc_increase=sum(layer<0, na.rm = T)/length(na.omit(layer)),
            perc_decrease=sum(layer>0, na.rm = T)/length(na.omit(layer)),
            perc_unchangede=sum(layer==0, na.rm = T)/length(na.omit(layer)),
            min= min(layer, na.rm = T),
            max= max(layer, na.rm = T),
            median= median(layer, na.rm = T),
            q25=quantile(layer,0.25, na.rm=T),
            q50=quantile(layer,0.5, na.rm=T),
            mean = mean(layer, na.rm = T))
diff_df_ssp585_2085_early_late_summary$pathway<-"ssp585"
diff_df_ssp585_2085_early_late_summary$model<-"median"
diff_df_ssp585_2085_early_late_summary$comparison<-"early_late"


summary_early_late<-rbind(diff_df_ssp126_2050_early_late_summary,
                          diff_df_ssp245_2050_early_late_summary,
                          diff_df_ssp370_2050_early_late_summary,
                          diff_df_ssp585_2050_early_late_summary,
                          diff_df_ssp126_2085_early_late_summary,
                          diff_df_ssp245_2085_early_late_summary,
                          diff_df_ssp370_2085_early_late_summary,
                          diff_df_ssp585_2085_early_late_summary)

#plot differences##
diff_plot_ssp126_2050_early_late<-rasterVis::levelplot(diff_raster_ssp126_2050_early_late$layer,
                                                 margin=F,
                                                 col.regions=byr(120),
                                                 #col.regions=viridis(100, direction = -1),
                                                 #at=shift_seq,
                                                 at=seq(-60, 60, length.out=120),
                                                 #main="quantile 10",
                                                 colorkey=F,
                                                 #scales=list(x=list(draw=FALSE)),
                                                 xlab="Longitude",
                                                 ylab="Latitude")+
  latticeExtra::layer({
    grid.text(x= 5.9, y=54.8, "SSP1-2.6",
              gp=gpar(fontsize=8), rot=0,
              default.units='native',
              just="left")
  })
diff_plot_ssp126_2050_early_late

#differences pessimistic, average and optimistic model####
summary(raster_early_ssp370_2085_quantile90)
summary(raster_early_ssp370_2085_quantile10)
summary(raster_early_ssp370_2085_median)

##pessimistic / optimistic####
diff_raster_early_ssp126_2050_quantile_10_quantile_90<-raster_early_ssp126_2050_quantile10$var1.pred_total - raster_early_ssp126_2050_quantile90$var1.pred_total
diff_raster_early_ssp245_2050_quantile_10_quantile_90<-raster_early_ssp245_2050_quantile10$var1.pred_total - raster_early_ssp245_2050_quantile90$var1.pred_total
diff_raster_early_ssp370_2050_quantile_10_quantile_90<-raster_early_ssp370_2050_quantile10$var1.pred_total - raster_early_ssp370_2050_quantile90$var1.pred_total
diff_raster_early_ssp585_2050_quantile_10_quantile_90<-raster_early_ssp585_2050_quantile10$var1.pred_total - raster_early_ssp585_2050_quantile90$var1.pred_total

diff_raster_early_ssp126_2085_quantile_10_quantile_90<-raster_early_ssp126_2085_quantile10$var1.pred_total - raster_early_ssp126_2085_quantile90$var1.pred_total
diff_raster_early_ssp245_2085_quantile_10_quantile_90<-raster_early_ssp245_2085_quantile10$var1.pred_total - raster_early_ssp245_2085_quantile90$var1.pred_total
diff_raster_early_ssp370_2085_quantile_10_quantile_90<-raster_early_ssp370_2085_quantile10$var1.pred_total - raster_early_ssp370_2085_quantile90$var1.pred_total
diff_raster_early_ssp585_2085_quantile_10_quantile_90<-raster_early_ssp585_2085_quantile10$var1.pred_total - raster_early_ssp585_2085_quantile90$var1.pred_total


diff_df_early_ssp126_2050_quantile_10_quantile_90<-as.data.frame(diff_raster_early_ssp126_2050_quantile_10_quantile_90, xy=T)
diff_df_early_ssp245_2050_quantile_10_quantile_90<-as.data.frame(diff_raster_early_ssp245_2050_quantile_10_quantile_90, xy=T)
diff_df_early_ssp370_2050_quantile_10_quantile_90<-as.data.frame(diff_raster_early_ssp370_2050_quantile_10_quantile_90, xy=T)
diff_df_early_ssp585_2050_quantile_10_quantile_90<-as.data.frame(diff_raster_early_ssp585_2050_quantile_10_quantile_90, xy=T)

diff_df_early_ssp126_2085_quantile_10_quantile_90<-as.data.frame(diff_raster_early_ssp126_2085_quantile_10_quantile_90, xy=T)
diff_df_early_ssp245_2085_quantile_10_quantile_90<-as.data.frame(diff_raster_early_ssp245_2085_quantile_10_quantile_90, xy=T)
diff_df_early_ssp370_2085_quantile_10_quantile_90<-as.data.frame(diff_raster_early_ssp370_2085_quantile_10_quantile_90, xy=T)
diff_df_early_ssp585_2085_quantile_10_quantile_90<-as.data.frame(diff_raster_early_ssp585_2085_quantile_10_quantile_90, xy=T)

#summarize differences
diff_df_early_ssp126_2050_quantile_10_quantile_90_summary<-diff_df_early_ssp126_2050_quantile_10_quantile_90%>%
  summarise(n_increase=sum(layer<0, na.rm = T),
            n_decrease=sum(layer>0, na.rm = T),
            n_unchanged=sum(layer==0, na.rm = T),
            perc_increase=sum(layer<0, na.rm = T)/length(na.omit(layer)),
            perc_decrease=sum(layer>0, na.rm = T)/length(na.omit(layer)),
            perc_unchangede=sum(layer==0, na.rm = T)/length(na.omit(layer)),
            min= min(layer, na.rm = T),
            max= max(layer, na.rm = T),
            median= median(layer, na.rm = T),
            mean = mean(layer, na.rm = T))
diff_df_early_ssp126_2050_quantile_10_quantile_90_summary$pathway<-"ssp126"
diff_df_early_ssp126_2050_quantile_10_quantile_90_summary$model<-"versch"
diff_df_early_ssp126_2050_quantile_10_quantile_90_summary$comparison<-"quantile_10_quantile_90"

diff_df_early_ssp245_2050_quantile_10_quantile_90_summary<-diff_df_early_ssp245_2050_quantile_10_quantile_90%>%
  summarise(n_increase=sum(layer<0, na.rm = T),
            n_decrease=sum(layer>0, na.rm = T),
            n_unchanged=sum(layer==0, na.rm = T),
            perc_increase=sum(layer<0, na.rm = T)/length(na.omit(layer)),
            perc_decrease=sum(layer>0, na.rm = T)/length(na.omit(layer)),
            perc_unchangede=sum(layer==0, na.rm = T)/length(na.omit(layer)),
            min= min(layer, na.rm = T),
            max= max(layer, na.rm = T),
            median= median(layer, na.rm = T),
            mean = mean(layer, na.rm = T))
diff_df_early_ssp245_2050_quantile_10_quantile_90_summary$pathway<-"ssp245"
diff_df_early_ssp245_2050_quantile_10_quantile_90_summary$model<-"versch"
diff_df_early_ssp245_2050_quantile_10_quantile_90_summary$comparison<-"quantile_10_quantile_90"

diff_df_early_ssp370_2050_quantile_10_quantile_90_summary<-diff_df_early_ssp370_2050_quantile_10_quantile_90%>%
  summarise(n_increase=sum(layer<0, na.rm = T),
            n_decrease=sum(layer>0, na.rm = T),
            n_unchanged=sum(layer==0, na.rm = T),
            perc_increase=sum(layer<0, na.rm = T)/length(na.omit(layer)),
            perc_decrease=sum(layer>0, na.rm = T)/length(na.omit(layer)),
            perc_unchangede=sum(layer==0, na.rm = T)/length(na.omit(layer)),
            min= min(layer, na.rm = T),
            max= max(layer, na.rm = T),
            median= median(layer, na.rm = T),
            mean = mean(layer, na.rm = T))
diff_df_early_ssp370_2050_quantile_10_quantile_90_summary$pathway<-"ssp370"
diff_df_early_ssp370_2050_quantile_10_quantile_90_summary$model<-"versch"
diff_df_early_ssp370_2050_quantile_10_quantile_90_summary$comparison<-"quantile_10_quantile_90"

diff_df_early_ssp585_2050_quantile_10_quantile_90_summary<-diff_df_early_ssp585_2050_quantile_10_quantile_90%>%
  summarise(n_increase=sum(layer<0, na.rm = T),
            n_decrease=sum(layer>0, na.rm = T),
            n_unchanged=sum(layer==0, na.rm = T),
            perc_increase=sum(layer<0, na.rm = T)/length(na.omit(layer)),
            perc_decrease=sum(layer>0, na.rm = T)/length(na.omit(layer)),
            perc_unchangede=sum(layer==0, na.rm = T)/length(na.omit(layer)),
            min= min(layer, na.rm = T),
            max= max(layer, na.rm = T),
            median= median(layer, na.rm = T),
            mean = mean(layer, na.rm = T))
diff_df_early_ssp585_2050_quantile_10_quantile_90_summary$pathway<-"ssp585"
diff_df_early_ssp585_2050_quantile_10_quantile_90_summary$model<-"versch"
diff_df_early_ssp585_2050_quantile_10_quantile_90_summary$comparison<-"quantile_10_quantile_90"


diff_df_early_ssp126_2085_quantile_10_quantile_90_summary<-diff_df_early_ssp126_2085_quantile_10_quantile_90%>%
  summarise(n_increase=sum(layer<0, na.rm = T),
            n_decrease=sum(layer>0, na.rm = T),
            n_unchanged=sum(layer==0, na.rm = T),
            perc_increase=sum(layer<0, na.rm = T)/length(na.omit(layer)),
            perc_decrease=sum(layer>0, na.rm = T)/length(na.omit(layer)),
            perc_unchangede=sum(layer==0, na.rm = T)/length(na.omit(layer)),
            min= min(layer, na.rm = T),
            max= max(layer, na.rm = T),
            median= median(layer, na.rm = T),
            mean = mean(layer, na.rm = T))
diff_df_early_ssp126_2085_quantile_10_quantile_90_summary$pathway<-"ssp126"
diff_df_early_ssp126_2085_quantile_10_quantile_90_summary$model<-"versch"
diff_df_early_ssp126_2085_quantile_10_quantile_90_summary$comparison<-"quantile_10_quantile_90"

diff_df_early_ssp245_2085_quantile_10_quantile_90_summary<-diff_df_early_ssp245_2085_quantile_10_quantile_90%>%
  summarise(n_increase=sum(layer<0, na.rm = T),
            n_decrease=sum(layer>0, na.rm = T),
            n_unchanged=sum(layer==0, na.rm = T),
            perc_increase=sum(layer<0, na.rm = T)/length(na.omit(layer)),
            perc_decrease=sum(layer>0, na.rm = T)/length(na.omit(layer)),
            perc_unchangede=sum(layer==0, na.rm = T)/length(na.omit(layer)),
            min= min(layer, na.rm = T),
            max= max(layer, na.rm = T),
            median= median(layer, na.rm = T),
            mean = mean(layer, na.rm = T))
diff_df_early_ssp245_2085_quantile_10_quantile_90_summary$pathway<-"ssp245"
diff_df_early_ssp245_2085_quantile_10_quantile_90_summary$model<-"versch"
diff_df_early_ssp245_2085_quantile_10_quantile_90_summary$comparison<-"quantile_10_quantile_90"

diff_df_early_ssp370_2085_quantile_10_quantile_90_summary<-diff_df_early_ssp370_2085_quantile_10_quantile_90%>%
  summarise(n_increase=sum(layer<0, na.rm = T),
            n_decrease=sum(layer>0, na.rm = T),
            n_unchanged=sum(layer==0, na.rm = T),
            perc_increase=sum(layer<0, na.rm = T)/length(na.omit(layer)),
            perc_decrease=sum(layer>0, na.rm = T)/length(na.omit(layer)),
            perc_unchangede=sum(layer==0, na.rm = T)/length(na.omit(layer)),
            min= min(layer, na.rm = T),
            max= max(layer, na.rm = T),
            median= median(layer, na.rm = T),
            mean = mean(layer, na.rm = T))
diff_df_early_ssp370_2085_quantile_10_quantile_90_summary$pathway<-"ssp370"
diff_df_early_ssp370_2085_quantile_10_quantile_90_summary$model<-"versch"
diff_df_early_ssp370_2085_quantile_10_quantile_90_summary$comparison<-"quantile_10_quantile_90"

diff_df_early_ssp585_2085_quantile_10_quantile_90_summary<-diff_df_early_ssp585_2085_quantile_10_quantile_90%>%
  summarise(n_increase=sum(layer<0, na.rm = T),
            n_decrease=sum(layer>0, na.rm = T),
            n_unchanged=sum(layer==0, na.rm = T),
            perc_increase=sum(layer<0, na.rm = T)/length(na.omit(layer)),
            perc_decrease=sum(layer>0, na.rm = T)/length(na.omit(layer)),
            perc_unchangede=sum(layer==0, na.rm = T)/length(na.omit(layer)),
            min= min(layer, na.rm = T),
            max= max(layer, na.rm = T),
            median= median(layer, na.rm = T),
            mean = mean(layer, na.rm = T))
diff_df_early_ssp585_2085_quantile_10_quantile_90_summary$pathway<-"ssp585"
diff_df_early_ssp585_2085_quantile_10_quantile_90_summary$model<-"versch"
diff_df_early_ssp585_2085_quantile_10_quantile_90_summary$comparison<-"quantile_10_quantile_90"

##optimistic / average ####
diff_raster_early_ssp126_2050_quantile_10_median<-raster_early_ssp126_2050_quantile10$var1.pred_total - raster_early_ssp126_2050_median$var1.pred_total
diff_raster_early_ssp245_2050_quantile_10_median<-raster_early_ssp245_2050_quantile10$var1.pred_total - raster_early_ssp245_2050_median$var1.pred_total
diff_raster_early_ssp370_2050_quantile_10_median<-raster_early_ssp370_2050_quantile10$var1.pred_total - raster_early_ssp370_2050_median$var1.pred_total
diff_raster_early_ssp585_2050_quantile_10_median<-raster_early_ssp585_2050_quantile10$var1.pred_total - raster_early_ssp585_2050_median$var1.pred_total

diff_raster_early_ssp126_2085_quantile_10_median<-raster_early_ssp126_2085_quantile10$var1.pred_total - raster_early_ssp126_2085_median$var1.pred_total
diff_raster_early_ssp245_2085_quantile_10_median<-raster_early_ssp245_2085_quantile10$var1.pred_total - raster_early_ssp245_2085_median$var1.pred_total
diff_raster_early_ssp370_2085_quantile_10_median<-raster_early_ssp370_2085_quantile10$var1.pred_total - raster_early_ssp370_2085_median$var1.pred_total
diff_raster_early_ssp585_2085_quantile_10_median<-raster_early_ssp585_2085_quantile10$var1.pred_total - raster_early_ssp585_2085_median$var1.pred_total


diff_df_early_ssp126_2050_quantile_10_median<-as.data.frame(diff_raster_early_ssp126_2050_quantile_10_median, xy=T)
diff_df_early_ssp245_2050_quantile_10_median<-as.data.frame(diff_raster_early_ssp245_2050_quantile_10_median, xy=T)
diff_df_early_ssp370_2050_quantile_10_median<-as.data.frame(diff_raster_early_ssp370_2050_quantile_10_median, xy=T)
diff_df_early_ssp585_2050_quantile_10_median<-as.data.frame(diff_raster_early_ssp585_2050_quantile_10_median, xy=T)

diff_df_early_ssp126_2085_quantile_10_median<-as.data.frame(diff_raster_early_ssp126_2085_quantile_10_median, xy=T)
diff_df_early_ssp245_2085_quantile_10_median<-as.data.frame(diff_raster_early_ssp245_2085_quantile_10_median, xy=T)
diff_df_early_ssp370_2085_quantile_10_median<-as.data.frame(diff_raster_early_ssp370_2085_quantile_10_median, xy=T)
diff_df_early_ssp585_2085_quantile_10_median<-as.data.frame(diff_raster_early_ssp585_2085_quantile_10_median, xy=T)

diff_df_early_ssp126_2050_quantile_10_median_summary<-diff_df_early_ssp126_2050_quantile_10_median%>%
  summarise(n_increase=sum(layer<0, na.rm = T),
            n_decrease=sum(layer>0, na.rm = T),
            n_unchanged=sum(layer==0, na.rm = T),
            perc_increase=sum(layer<0, na.rm = T)/length(na.omit(layer)),
            perc_decrease=sum(layer>0, na.rm = T)/length(na.omit(layer)),
            perc_unchangede=sum(layer==0, na.rm = T)/length(na.omit(layer)),
            min= min(layer, na.rm = T),
            max= max(layer, na.rm = T),
            median= median(layer, na.rm = T),
            mean = mean(layer, na.rm = T))
diff_df_early_ssp126_2050_quantile_10_median_summary$pathway<-"ssp126"
diff_df_early_ssp126_2050_quantile_10_median_summary$model<-"versch"
diff_df_early_ssp126_2050_quantile_10_median_summary$comparison<-"quantile_10_median"

diff_df_early_ssp245_2050_quantile_10_median_summary<-diff_df_early_ssp245_2050_quantile_10_median%>%
  summarise(n_increase=sum(layer<0, na.rm = T),
            n_decrease=sum(layer>0, na.rm = T),
            n_unchanged=sum(layer==0, na.rm = T),
            perc_increase=sum(layer<0, na.rm = T)/length(na.omit(layer)),
            perc_decrease=sum(layer>0, na.rm = T)/length(na.omit(layer)),
            perc_unchangede=sum(layer==0, na.rm = T)/length(na.omit(layer)),
            min= min(layer, na.rm = T),
            max= max(layer, na.rm = T),
            median= median(layer, na.rm = T),
            mean = mean(layer, na.rm = T))
diff_df_early_ssp245_2050_quantile_10_median_summary$pathway<-"ssp245"
diff_df_early_ssp245_2050_quantile_10_median_summary$model<-"versch"
diff_df_early_ssp245_2050_quantile_10_median_summary$comparison<-"quantile_10_median"

diff_df_early_ssp370_2050_quantile_10_median_summary<-diff_df_early_ssp370_2050_quantile_10_median%>%
  summarise(n_increase=sum(layer<0, na.rm = T),
            n_decrease=sum(layer>0, na.rm = T),
            n_unchanged=sum(layer==0, na.rm = T),
            perc_increase=sum(layer<0, na.rm = T)/length(na.omit(layer)),
            perc_decrease=sum(layer>0, na.rm = T)/length(na.omit(layer)),
            perc_unchangede=sum(layer==0, na.rm = T)/length(na.omit(layer)),
            min= min(layer, na.rm = T),
            max= max(layer, na.rm = T),
            median= median(layer, na.rm = T),
            mean = mean(layer, na.rm = T))
diff_df_early_ssp370_2050_quantile_10_median_summary$pathway<-"ssp370"
diff_df_early_ssp370_2050_quantile_10_median_summary$model<-"versch"
diff_df_early_ssp370_2050_quantile_10_median_summary$comparison<-"quantile_10_median"

diff_df_early_ssp585_2050_quantile_10_median_summary<-diff_df_early_ssp585_2050_quantile_10_median%>%
  summarise(n_increase=sum(layer<0, na.rm = T),
            n_decrease=sum(layer>0, na.rm = T),
            n_unchanged=sum(layer==0, na.rm = T),
            perc_increase=sum(layer<0, na.rm = T)/length(na.omit(layer)),
            perc_decrease=sum(layer>0, na.rm = T)/length(na.omit(layer)),
            perc_unchangede=sum(layer==0, na.rm = T)/length(na.omit(layer)),
            min= min(layer, na.rm = T),
            max= max(layer, na.rm = T),
            median= median(layer, na.rm = T),
            mean = mean(layer, na.rm = T))
diff_df_early_ssp585_2050_quantile_10_median_summary$pathway<-"ssp585"
diff_df_early_ssp585_2050_quantile_10_median_summary$model<-"versch"
diff_df_early_ssp585_2050_quantile_10_median_summary$comparison<-"quantile_10_median"


diff_df_early_ssp126_2085_quantile_10_median_summary<-diff_df_early_ssp126_2085_quantile_10_median%>%
  summarise(n_increase=sum(layer<0, na.rm = T),
            n_decrease=sum(layer>0, na.rm = T),
            n_unchanged=sum(layer==0, na.rm = T),
            perc_increase=sum(layer<0, na.rm = T)/length(na.omit(layer)),
            perc_decrease=sum(layer>0, na.rm = T)/length(na.omit(layer)),
            perc_unchangede=sum(layer==0, na.rm = T)/length(na.omit(layer)),
            min= min(layer, na.rm = T),
            max= max(layer, na.rm = T),
            median= median(layer, na.rm = T),
            mean = mean(layer, na.rm = T))
diff_df_early_ssp126_2085_quantile_10_median_summary$pathway<-"ssp126"
diff_df_early_ssp126_2085_quantile_10_median_summary$model<-"versch"
diff_df_early_ssp126_2085_quantile_10_median_summary$comparison<-"quantile_10_median"

diff_df_early_ssp245_2085_quantile_10_median_summary<-diff_df_early_ssp245_2085_quantile_10_median%>%
  summarise(n_increase=sum(layer<0, na.rm = T),
            n_decrease=sum(layer>0, na.rm = T),
            n_unchanged=sum(layer==0, na.rm = T),
            perc_increase=sum(layer<0, na.rm = T)/length(na.omit(layer)),
            perc_decrease=sum(layer>0, na.rm = T)/length(na.omit(layer)),
            perc_unchangede=sum(layer==0, na.rm = T)/length(na.omit(layer)),
            min= min(layer, na.rm = T),
            max= max(layer, na.rm = T),
            median= median(layer, na.rm = T),
            mean = mean(layer, na.rm = T))
diff_df_early_ssp245_2085_quantile_10_median_summary$pathway<-"ssp245"
diff_df_early_ssp245_2085_quantile_10_median_summary$model<-"versch"
diff_df_early_ssp245_2085_quantile_10_median_summary$comparison<-"quantile_10_median"

diff_df_early_ssp370_2085_quantile_10_median_summary<-diff_df_early_ssp370_2085_quantile_10_median%>%
  summarise(n_increase=sum(layer<0, na.rm = T),
            n_decrease=sum(layer>0, na.rm = T),
            n_unchanged=sum(layer==0, na.rm = T),
            perc_increase=sum(layer<0, na.rm = T)/length(na.omit(layer)),
            perc_decrease=sum(layer>0, na.rm = T)/length(na.omit(layer)),
            perc_unchangede=sum(layer==0, na.rm = T)/length(na.omit(layer)),
            min= min(layer, na.rm = T),
            max= max(layer, na.rm = T),
            median= median(layer, na.rm = T),
            mean = mean(layer, na.rm = T))
diff_df_early_ssp370_2085_quantile_10_median_summary$pathway<-"ssp370"
diff_df_early_ssp370_2085_quantile_10_median_summary$model<-"versch"
diff_df_early_ssp370_2085_quantile_10_median_summary$comparison<-"quantile_10_median"

diff_df_early_ssp585_2085_quantile_10_median_summary<-diff_df_early_ssp585_2085_quantile_10_median%>%
  summarise(n_increase=sum(layer<0, na.rm = T),
            n_decrease=sum(layer>0, na.rm = T),
            n_unchanged=sum(layer==0, na.rm = T),
            perc_increase=sum(layer<0, na.rm = T)/length(na.omit(layer)),
            perc_decrease=sum(layer>0, na.rm = T)/length(na.omit(layer)),
            perc_unchangede=sum(layer==0, na.rm = T)/length(na.omit(layer)),
            min= min(layer, na.rm = T),
            max= max(layer, na.rm = T),
            median= median(layer, na.rm = T),
            mean = mean(layer, na.rm = T))
diff_df_early_ssp585_2085_quantile_10_median_summary$pathway<-"ssp585"
diff_df_early_ssp585_2085_quantile_10_median_summary$model<-"versch"
diff_df_early_ssp585_2085_quantile_10_median_summary$comparison<-"quantile_10_median"

##average / pessimistic####

diff_raster_early_ssp126_2050_median_quantile_90<-raster_early_ssp126_2050_median$var1.pred_total - raster_early_ssp126_2050_quantile90$var1.pred_total
diff_raster_early_ssp245_2050_median_quantile_90<-raster_early_ssp245_2050_median$var1.pred_total - raster_early_ssp245_2050_quantile90$var1.pred_total
diff_raster_early_ssp370_2050_median_quantile_90<-raster_early_ssp370_2050_median$var1.pred_total - raster_early_ssp370_2050_quantile90$var1.pred_total
diff_raster_early_ssp585_2050_median_quantile_90<-raster_early_ssp585_2050_median$var1.pred_total - raster_early_ssp585_2050_quantile90$var1.pred_total

diff_raster_early_ssp126_2085_median_quantile_90<-raster_early_ssp126_2085_median$var1.pred_total - raster_early_ssp126_2085_quantile90$var1.pred_total
diff_raster_early_ssp245_2085_median_quantile_90<-raster_early_ssp245_2085_median$var1.pred_total - raster_early_ssp245_2085_quantile90$var1.pred_total
diff_raster_early_ssp370_2085_median_quantile_90<-raster_early_ssp370_2085_median$var1.pred_total - raster_early_ssp370_2085_quantile90$var1.pred_total
diff_raster_early_ssp585_2085_median_quantile_90<-raster_early_ssp585_2085_median$var1.pred_total - raster_early_ssp585_2085_quantile90$var1.pred_total


diff_df_early_ssp126_2050_median_quantile_90<-as.data.frame(diff_raster_early_ssp126_2050_median_quantile_90, xy=T)
diff_df_early_ssp245_2050_median_quantile_90<-as.data.frame(diff_raster_early_ssp245_2050_median_quantile_90, xy=T)
diff_df_early_ssp370_2050_median_quantile_90<-as.data.frame(diff_raster_early_ssp370_2050_median_quantile_90, xy=T)
diff_df_early_ssp585_2050_median_quantile_90<-as.data.frame(diff_raster_early_ssp585_2050_median_quantile_90, xy=T)

diff_df_early_ssp126_2085_median_quantile_90<-as.data.frame(diff_raster_early_ssp126_2085_median_quantile_90, xy=T)
diff_df_early_ssp245_2085_median_quantile_90<-as.data.frame(diff_raster_early_ssp245_2085_median_quantile_90, xy=T)
diff_df_early_ssp370_2085_median_quantile_90<-as.data.frame(diff_raster_early_ssp370_2085_median_quantile_90, xy=T)
diff_df_early_ssp585_2085_median_quantile_90<-as.data.frame(diff_raster_early_ssp585_2085_median_quantile_90, xy=T)

diff_df_early_ssp126_2050_median_quantile_90_summary<-diff_df_early_ssp126_2050_median_quantile_90%>%
  summarise(n_increase=sum(layer<0, na.rm = T),
            n_decrease=sum(layer>0, na.rm = T),
            n_unchanged=sum(layer==0, na.rm = T),
            perc_increase=sum(layer<0, na.rm = T)/length(na.omit(layer)),
            perc_decrease=sum(layer>0, na.rm = T)/length(na.omit(layer)),
            perc_unchangede=sum(layer==0, na.rm = T)/length(na.omit(layer)),
            min= min(layer, na.rm = T),
            max= max(layer, na.rm = T),
            median= median(layer, na.rm = T),
            mean = mean(layer, na.rm = T))
diff_df_early_ssp126_2050_median_quantile_90_summary$pathway<-"ssp126"
diff_df_early_ssp126_2050_median_quantile_90_summary$model<-"versch"
diff_df_early_ssp126_2050_median_quantile_90_summary$comparison<-"median_quantile_90"

diff_df_early_ssp245_2050_median_quantile_90_summary<-diff_df_early_ssp245_2050_median_quantile_90%>%
  summarise(n_increase=sum(layer<0, na.rm = T),
            n_decrease=sum(layer>0, na.rm = T),
            n_unchanged=sum(layer==0, na.rm = T),
            perc_increase=sum(layer<0, na.rm = T)/length(na.omit(layer)),
            perc_decrease=sum(layer>0, na.rm = T)/length(na.omit(layer)),
            perc_unchangede=sum(layer==0, na.rm = T)/length(na.omit(layer)),
            min= min(layer, na.rm = T),
            max= max(layer, na.rm = T),
            median= median(layer, na.rm = T),
            mean = mean(layer, na.rm = T))
diff_df_early_ssp245_2050_median_quantile_90_summary$pathway<-"ssp245"
diff_df_early_ssp245_2050_median_quantile_90_summary$model<-"versch"
diff_df_early_ssp245_2050_median_quantile_90_summary$comparison<-"median_quantile_90"

diff_df_early_ssp370_2050_median_quantile_90_summary<-diff_df_early_ssp370_2050_median_quantile_90%>%
  summarise(n_increase=sum(layer<0, na.rm = T),
            n_decrease=sum(layer>0, na.rm = T),
            n_unchanged=sum(layer==0, na.rm = T),
            perc_increase=sum(layer<0, na.rm = T)/length(na.omit(layer)),
            perc_decrease=sum(layer>0, na.rm = T)/length(na.omit(layer)),
            perc_unchangede=sum(layer==0, na.rm = T)/length(na.omit(layer)),
            min= min(layer, na.rm = T),
            max= max(layer, na.rm = T),
            median= median(layer, na.rm = T),
            mean = mean(layer, na.rm = T))
diff_df_early_ssp370_2050_median_quantile_90_summary$pathway<-"ssp370"
diff_df_early_ssp370_2050_median_quantile_90_summary$model<-"versch"
diff_df_early_ssp370_2050_median_quantile_90_summary$comparison<-"median_quantile_90"

diff_df_early_ssp585_2050_median_quantile_90_summary<-diff_df_early_ssp585_2050_median_quantile_90%>%
  summarise(n_increase=sum(layer<0, na.rm = T),
            n_decrease=sum(layer>0, na.rm = T),
            n_unchanged=sum(layer==0, na.rm = T),
            perc_increase=sum(layer<0, na.rm = T)/length(na.omit(layer)),
            perc_decrease=sum(layer>0, na.rm = T)/length(na.omit(layer)),
            perc_unchangede=sum(layer==0, na.rm = T)/length(na.omit(layer)),
            min= min(layer, na.rm = T),
            max= max(layer, na.rm = T),
            median= median(layer, na.rm = T),
            mean = mean(layer, na.rm = T))
diff_df_early_ssp585_2050_median_quantile_90_summary$pathway<-"ssp585"
diff_df_early_ssp585_2050_median_quantile_90_summary$model<-"versch"
diff_df_early_ssp585_2050_median_quantile_90_summary$comparison<-"median_quantile_90"


diff_df_early_ssp126_2085_median_quantile_90_summary<-diff_df_early_ssp126_2085_median_quantile_90%>%
  summarise(n_increase=sum(layer<0, na.rm = T),
            n_decrease=sum(layer>0, na.rm = T),
            n_unchanged=sum(layer==0, na.rm = T),
            perc_increase=sum(layer<0, na.rm = T)/length(na.omit(layer)),
            perc_decrease=sum(layer>0, na.rm = T)/length(na.omit(layer)),
            perc_unchangede=sum(layer==0, na.rm = T)/length(na.omit(layer)),
            min= min(layer, na.rm = T),
            max= max(layer, na.rm = T),
            median= median(layer, na.rm = T),
            mean = mean(layer, na.rm = T))
diff_df_early_ssp126_2085_median_quantile_90_summary$pathway<-"ssp126"
diff_df_early_ssp126_2085_median_quantile_90_summary$model<-"versch"
diff_df_early_ssp126_2085_median_quantile_90_summary$comparison<-"median_quantile_90"

diff_df_early_ssp245_2085_median_quantile_90_summary<-diff_df_early_ssp245_2085_median_quantile_90%>%
  summarise(n_increase=sum(layer<0, na.rm = T),
            n_decrease=sum(layer>0, na.rm = T),
            n_unchanged=sum(layer==0, na.rm = T),
            perc_increase=sum(layer<0, na.rm = T)/length(na.omit(layer)),
            perc_decrease=sum(layer>0, na.rm = T)/length(na.omit(layer)),
            perc_unchangede=sum(layer==0, na.rm = T)/length(na.omit(layer)),
            min= min(layer, na.rm = T),
            max= max(layer, na.rm = T),
            median= median(layer, na.rm = T),
            mean = mean(layer, na.rm = T))
diff_df_early_ssp245_2085_median_quantile_90_summary$pathway<-"ssp245"
diff_df_early_ssp245_2085_median_quantile_90_summary$model<-"versch"
diff_df_early_ssp245_2085_median_quantile_90_summary$comparison<-"median_quantile_90"

diff_df_early_ssp370_2085_median_quantile_90_summary<-diff_df_early_ssp370_2085_median_quantile_90%>%
  summarise(n_increase=sum(layer<0, na.rm = T),
            n_decrease=sum(layer>0, na.rm = T),
            n_unchanged=sum(layer==0, na.rm = T),
            perc_increase=sum(layer<0, na.rm = T)/length(na.omit(layer)),
            perc_decrease=sum(layer>0, na.rm = T)/length(na.omit(layer)),
            perc_unchangede=sum(layer==0, na.rm = T)/length(na.omit(layer)),
            min= min(layer, na.rm = T),
            max= max(layer, na.rm = T),
            median= median(layer, na.rm = T),
            mean = mean(layer, na.rm = T))
diff_df_early_ssp370_2085_median_quantile_90_summary$pathway<-"ssp370"
diff_df_early_ssp370_2085_median_quantile_90_summary$model<-"versch"
diff_df_early_ssp370_2085_median_quantile_90_summary$comparison<-"median_quantile_90"

diff_df_early_ssp585_2085_median_quantile_90_summary<-diff_df_early_ssp585_2085_median_quantile_90%>%
  summarise(n_increase=sum(layer<0, na.rm = T),
            n_decrease=sum(layer>0, na.rm = T),
            n_unchanged=sum(layer==0, na.rm = T),
            perc_increase=sum(layer<0, na.rm = T)/length(na.omit(layer)),
            perc_decrease=sum(layer>0, na.rm = T)/length(na.omit(layer)),
            perc_unchangede=sum(layer==0, na.rm = T)/length(na.omit(layer)),
            min= min(layer, na.rm = T),
            max= max(layer, na.rm = T),
            median= median(layer, na.rm = T),
            mean = mean(layer, na.rm = T))
diff_df_early_ssp585_2085_median_quantile_90_summary$pathway<-"ssp585"
diff_df_early_ssp585_2085_median_quantile_90_summary$model<-"versch"
diff_df_early_ssp585_2085_median_quantile_90_summary$comparison<-"median_quantile_90"

summary_q10_median_q90<-rbind(diff_df_early_ssp126_2050_quantile_10_median_summary,
                          diff_df_early_ssp126_2050_median_quantile_90_summary,
                          diff_df_early_ssp126_2050_quantile_10_quantile_90_summary,
                          diff_df_early_ssp245_2050_quantile_10_median_summary,
                          diff_df_early_ssp245_2050_median_quantile_90_summary,
                          diff_df_early_ssp245_2050_quantile_10_quantile_90_summary,
                          diff_df_early_ssp370_2050_quantile_10_median_summary,
                          diff_df_early_ssp370_2050_median_quantile_90_summary,
                          diff_df_early_ssp370_2050_quantile_10_quantile_90_summary,
                          diff_df_early_ssp585_2050_quantile_10_median_summary,
                          diff_df_early_ssp585_2050_median_quantile_90_summary,
                          diff_df_early_ssp585_2050_quantile_10_quantile_90_summary,
                          diff_df_early_ssp126_2085_quantile_10_median_summary,
                          diff_df_early_ssp126_2085_median_quantile_90_summary,
                          diff_df_early_ssp126_2085_quantile_10_quantile_90_summary,
                          diff_df_early_ssp245_2085_quantile_10_median_summary,
                          diff_df_early_ssp245_2085_median_quantile_90_summary,
                          diff_df_early_ssp245_2085_quantile_10_quantile_90_summary,
                          diff_df_early_ssp370_2085_quantile_10_median_summary,
                          diff_df_early_ssp370_2085_median_quantile_90_summary,
                          diff_df_early_ssp370_2085_quantile_10_quantile_90_summary,
                          diff_df_early_ssp585_2085_quantile_10_median_summary,
                          diff_df_early_ssp585_2085_median_quantile_90_summary,
                          diff_df_early_ssp585_2085_quantile_10_quantile_90_summary)
summary_q10_median_q90$simyear<-c(rep(2050,12),rep(2085,12))
