######SCript 0.1 ----just plotting territory zones
library(sf)
library(adehabitatHR)
library(rgdal)
library(dplyr)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggspatial)
library(cowplot)
library(ggplot2)
library(ggmap)

#First get stadimap key

Sys.setenv(GGMAP_STADIA_API_KEY = "f9d511a5-33ca-42aa-a527-943c07b6fe0f")
register_stadiamaps("f9d511a5-33ca-42aa-a527-943c07b6fe0f", write = FALSE)



#Dont need all packages

# Combine all cleaned data
CleanAllTogether22plot <- rbind(
  Andy22Cleandates[, c("DateTime", "Individual", "Latitude", "Longitude")],
  SWY22Cleandates[, c("DateTime", "Individual", "Latitude", "Longitude")],
  Cathy22Cleandatesplot[, c("DateTime", "Individual", "Latitude", "Longitude")],
  RD22Cleandates[, c("DateTime", "Individual", "Latitude", "Longitude")],
  Tekka22Cleandatesplot[, c("DateTime", "Individual", "Latitude", "Longitude")]
)

#rename
CleanPlots <- CleanAllTogether22plot %>%
  mutate(Individual = case_when(
    Individual == "Andy22" ~ "HN",
    Individual == "SWY22" ~ "WT",
    Individual == "Cathy22" ~ "SL",
    Individual == "RD22" ~ "MB",
    Individual == "Tekka22" ~ "YG",
  ))

CleanPlots$Individual <- factor(CleanPlots$Individual, levels = c("YG","SL", "MB", "WT","HN"))

CleanPlots <- CleanPlots %>%
  arrange(Individual)

CleanPLotSf <- st_as_sf(CleanPlots,coords = c("Longitude", "Latitude"), crs = "epsg:7842")


###### Plot all data together for illustration of territories

###Get aus inset map
australia_map <- ne_states(country = "Australia", returnclass = "sf")

####Create a little point where we are. 
#get bounding box of locations: 
bbox <- st_bbox(CleanPLotSf)

#expand by tiny bit
expanded_bbox <- bbox + c(-0.01, -0.01, 0.01, 0.01)

bbox_df <- data.frame(
  xmin = expanded_bbox[["xmin"]],
  xmax = expanded_bbox[["xmax"]],
  ymin = expanded_bbox[["ymin"]],
  ymax = expanded_bbox[["ymax"]]
)

##then make true polygon
bbox_polygon <- st_as_sf(
  st_sfc(
    st_polygon(list(matrix(c(
      bbox_df$xmin, bbox_df$ymin,
      bbox_df$xmin, bbox_df$ymax,
      bbox_df$xmax, bbox_df$ymax,
      bbox_df$xmax, bbox_df$ymin,
      bbox_df$xmin, bbox_df$ymin
    ), ncol = 2, byrow = TRUE)))
  )
)

st_crs(bbox_polygon)  # Check CRS of your cleaned points

st_crs(bbox_polygon) <- st_crs(australia_map)

australia_inset <- ggplot() +
  geom_sf(data = australia_map, fill = "lightgrey", color = "black") +
  geom_sf(data = bbox_polygon, color = "red", lwd=1) + ##This is the square showing where you are 
  coord_sf(xlim = c(140, 154), ylim = c(-39, -28)) +
  theme_minimal() +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, size = 0.6),         panel.spacing = margin(10, 10, 10, 10))

australia_inset



#get base map
map <- get_stadiamap(bbox = c(left = 152.12, 
                              bottom = -32.73,
                              right = 152.56, 
                              top = -32.38),
                     crop = TRUE,
                     zoom = 10, maptype = "stamen_terrain_background")



custom_colors <- c("HN" = "red", "WT" = "blue", "SL" = "brown", 
                   "MB" = "purple", "YG" = "orange")

FigureGPSLocations<- ggmap(map) +
  geom_point(data = CleanPlots, aes(x = Longitude, y = Latitude, color = Individual), size = 1.5) +
  scale_color_manual(values = custom_colors)+
  geom_point(data = trap_sf_wgs84, aes(x = Longitude, y = Latitude, shape = "Trap"), size = 3, color = "black", fill = "black", stroke = 2) +
  scale_shape_manual(name = "", values = c("Trap" = 19),labels = c("Trap" ="Camera trap\nstation")) + 
  labs(x = "Longitude", y = "Latitude", title = "", color = "Dingo pack") +
  theme_minimal()+
  theme(legend.text = element_text(size = 17),
        legend.title = element_text(size =17),
        axis.text = element_text(size=15),
        axis.title = element_text(size=16))+
  guides(
    color = guide_legend(override.aes = list(size = 3)), 
    shape = guide_legend(override.aes = list(size = 3)))

FigureGPSLocations


combinedfig <- ggdraw() +
  draw_plot(FigureGPSLocations, 0.0, 0.0, 1, 1)+
  draw_plot(australia_inset, 0.45, 0.1, 0.35, 0.35)     

combinedfig

png("StudyAreaCollarData.jpg", width = 11, height =9, res= 300, units = "in")

combinedfig

dev.off()

#Done that figure. 


#Now we'll plot the HRs and camera overlay, including which are IHR and OOR. 

###we'll clip study area. 


transformallall <- AllAllsf22
transformallall <- transformallall%>%st_set_crs(32756)%>%st_transform(crs = 4283)

borderhuh <- st_read("Raw Data/studyareaclipped.shp")
bordersimply <- st_buffer(borderhuh, 50,crs =28356)
bordersimply <- st_union(bordersimply)
st_crs(bordersimply)<-(28356)
bordersimplytrans <- st_transform(bordersimply, 4283)



#and then clip mcps to concave hull. 
clippedmcps <- st_intersection(transformallall,bordersimplytrans)



#create background map. 
map <- get_stadiamap(bbox = c(left = 152.12, 
                              bottom = -32.73,
                              right = 152.56, 
                              top = -32.38),
                     crop = TRUE,
                     zoom = 10, maptype = "stamen_terrain_background")

#and finally, give each point it's correct color: 

trap_sf22$Pack <- c("JB","WT","WT","SL","SL","MB","MB","MB","NA","NA","YG","YG","YG","YG","YG","YG","YG","SL","NA","MB","YG")
  
mcps <- ggmap(map) +
  # Plot home ranges
  geom_sf(data = clippedmcps %>% filter(TerritoryZone == 'Periph'),
          aes(fill = CollarPack),
          alpha = 0.3,
          color = "black",
          inherit.aes = FALSE) +
  
  # Plot traps
  geom_sf(data = trap_sf22,
          aes(shape = "Camera trap",color = Pack),
          show.legend = FALSE,
          size = 4,
          fill = "black",
          inherit.aes = FALSE) +
  
  # Labels and theme
  labs(title = "", 
       x = "Longitude", 
       y = "Latitude", 
       fill = "Dingo Pack",      # Title for the fill legend
       shape = "")      +       # No title for the shape legend

theme_minimal() +
  theme(legend.position = "right",
        legend.title = element_text(size = 17),
        legend.text = element_text(size = 17),
        axis.title = element_text(size = 17),
        axis.text = element_text(size =17),
        legend.box = "vertical",  # Stack legends vertically
        legend.box.margin = margin(t = 9, r = 0, b = 0, l = 0))
mcps


png("MCPs 95percent.jpg", width = 11, height =9, res= 300, units = "in")

mcps

dev.off()


#Combined from chad. 





FigureGPSLocations<- ggmap(map) +
  geom_point(data = CleanPlots, aes(x = Longitude, y = Latitude, color = Individual), size = 0.5) +
  scale_color_manual(values = custom_colors)+
  geom_point(data = trap_sf_wgs84, aes(x = Longitude, y = Latitude, shape = "Trap"), size = 3, color = "black", fill = "black", stroke = 2) +
  scale_shape_manual(name = "", values = c("Trap" = 19),labels = c("Trap" ="Camera trap\nstation")) + 
  labs(x = "Longitude", y = "Latitude", title = "", color = "Dingo pack") +
  geom_sf(data = clippedmcps %>% filter(TerritoryZone == 'Periph'),
          aes(fill = CollarPack),
          alpha = 0.2,
          color = "black",
          inherit.aes = FALSE,
          show.legend = FALSE) +
  scale_fill_manual(values = custom_colors)+
  theme_minimal()+
  theme(legend.text = element_text(size = 17),
        legend.title = element_text(size =17),
        axis.text = element_text(size=15),
        axis.title = element_text(size=16))+
  guides(
    color = guide_legend(override.aes = list(size = 3)), 
    shape = guide_legend(override.aes = list(size = 3)))

FigureGPSLocations


combinedfig <- ggdraw() +
  draw_plot(FigureGPSLocations, 0.0, 0.0, 1, 1)+
  draw_plot(australia_inset, 0.45, 0.1, 0.35, 0.35)     

combinedfig

png("StudyAreaCollarData.jpg", width = 11, height =9, res= 300, units = "in")

combinedfig

dev.off()





