library(sf)
library(ggspatial)
library(RColorBrewer)
library(ggplot2)
library(plyr)
library(ggrepel)
colombia <- st_read ("SHP/mpios_geo_ok.shp")
shp <- subset(colombia  , NOMBRE_DPT  == "CÓRDOBA")
colombia<- readOGR('mpios_geo_ok.shp',"mpios_geo_ok",use_iconv=TRUE, encoding="UTF-8")
shp <- colombia[colombia$NOMBRE_DPT=="CÓRDOBA",]
shpp = arrange(shp, NOM_MUNICI)
aleatorios = round(runif(30,0,100))
df   <- cbind(shpp, aleatorios)
dff <- st_centroid(df) 
dff <- cbind(dff, st_coordinates(st_centroid(dff$geometry)))
Map=ggplot()+
  geom_sf(data = df, aes(fill=aleatorios))+
  scale_fill_gradientn(colours = RColorBrewer::brewer.pal(n = 9, name = "YlOrRd"),name="Prevalencia \nen %")+
  guides(fill = guide_legend())+
  coord_sf(xlim = c(-77,-74), ylim = c(7.347091,9.447827))+
  annotation_north_arrow(location="tl",which_north="true",style=north_arrow_fancy_orienteering ())+
  ggspatial::annotation_scale(location = "br",bar_cols = c("grey60", "white"), text_family = "ArcherPro Book")+
  theme_bw()+
  geom_label_repel(data = dff, aes(x = X, y = Y, label = NOM_MUNICI,fill = aleatorios), 
                   family="serif", box.padding = unit(0.9, "lines"), size = 3, face = "bold",color = 'black',
                  point.padding = unit(0.5, "lines")) +
  theme(panel.background = element_rect(fill = NA),
        panel.grid.major = element_line(color = "gray",linetype = "dashed", size = 0.5),
        axis.text.y  = element_text(angle = 90,face="bold", color="black", size=9,family="serif"),
        axis.text.x  = element_text(face="bold", color="black", size=9,family="serif"),
        panel.border = element_rect(size = 1.5),
        legend.background = element_rect(fill = "gray80",colour = 'black', size = 0.5, linetype='solid'),
        legend.text =element_text(size=9, family="serif"),
        legend.position = c(0.90,0.15),
        legend.key.size = unit(0.2, "cm"), #alto de cuadrados de referencia
        legend.key.width = unit(0.5,"cm"), #ancho de cuadrados de referencia 
        legend.title = element_text(size=10, family="serif", face = "bold"))+
  scale_x_continuous(name=expression(paste("Longitude (",degree,")")))+
  scale_y_continuous(name=expression(paste("Latitude (",degree,")")))+
  annotate(geom = "text", x = -74.5, y = 9, label = "Análisis de prevalencia \nde COVID-19", 
           family="serif", color = "black", size = 4, face = "bold")
ggsave(plot = Map ,"Mapa.png", 
       units = "cm", width = 21,height = 17, dpi = 1000) 




