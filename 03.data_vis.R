library(tidyverse)
library(sf)
library(ggplot2)
library(gridExtra)
library(viridis)
library(corrplot)
library(lubridate)

rm(list = ls())

# 0. Create inputs----------------

load("trabajo/outputs/workspace_completo.RData")

rm(resultados, datos_finales)
gc()

squares$area <- st_area(squares)

final_data <- read.csv("trabajo/outputs/datos_finales.csv")
centroids <- st_centroid(squares)

polygons_squares <- ggplot()+
  geom_sf(data = squares, aes(geometry = geometry))+
  theme_void()
ggsave("trabajo/outputs/plots/poligons_squares.png")

# Load dam data
dam_aoulouz <- st_read("work/composition.gpkg", layer = "dam_Aoulouz") 
dam_aoulouz_utm <- st_transform(dam_aoulouz, st_crs(centroids))

centroids <- centroids %>%
  mutate(
    x_centroid = st_coordinates(.)[, "X"],
    y_centroid = st_coordinates(.)[, "Y"],
    distance_to_dam = as.numeric(st_distance(., st_centroid(dam_aoulouz_utm)))
  ) %>% 
  select(id, distance_to_dam) 

# Join to final_data
df <- final_data %>% 
  select(-distancia_represa) %>% 
  left_join(centroids, by = "id") 

dam_x  <- st_coordinates(dam_aoulouz_utm)[,1]
dam_y <- st_coordinates(dam_aoulouz_utm)[,2]

rm(list = c("centroids","dam_aoulouz",
            "final_data","extraer_ndvi",   
            "squares","squares_vect"))

save.image("trabajo/outputs/df_for_analysis.RData")



# 1. Data preparation -----------------------------------------------------

load("trabajo/outputs/df_for_analysis.RData")
precip_annual <- read.csv("trabajo/CHIRSPS/chirps_annual_precip_Aoulouz.csv")
precip_growing_month <- read.csv("trabajo/CHIRSPS/chirps_growing_season_monthly_precip_Aoulouz.csv")
precip_wet_season <- read.csv("trabajo/CHIRSPS/chirps_previous_wet_season_precip_Aoulouz.csv")


df_analysis <- df %>%
  mutate(
    distance_km = distance_to_dam / 1000,
    coord_x = st_coordinates(geometry)[,1],
    coord_y = st_coordinates(geometry)[,2],
    # angle can help to identify direction
    angle = atan2(coord_y - dam_y, coord_x - dam_x) * 180/pi,
    direction = case_when(
      angle >= -45 & angle < 45 ~ "East",
      angle >= 45 & angle < 135 ~ "North", 
      angle >= 135 | angle < -135 ~ "West",
      angle >= -135 & angle < -45 ~ "South"
    ),
    detailed_period = case_when(
      year < 1991 ~ "Pre-dam (1984-90)",
      year >= 1991 & year <= 2002 ~ "Aoulouz only (1991-2002)", 
      year > 2002 ~ "Full system (2003-10)"
    ),
    distance_zone = case_when(
      distance_km <= 2 ~ "Very close (0-2km)",
      distance_km <= 5 ~ "Close (2-5km)",
      distance_km <= 10 ~ "Medium (5-10km)",
      distance_km <= 15 ~ "Far (10-15km)",
      TRUE ~ "Very far (>15km)"
    ),
    # ADD VARIABLES FOR ECONOMETRIC ANALYSIS
    post_aoulouz = ifelse(year >= 1990, 1, 0),
    post_full_system = ifelse(year > 2002, 1, 0),
    # INTERACTIONS WITH DISTANCE
    dist_post_aoulouz = distance_km * post_aoulouz,
    dist_post_system = distance_km * post_full_system
  ) %>% 
  left_join(precip_annual, by = c("year"))

scale_factor <- 0.25 / max(precip_annual$annual_total_precip_mm, na.rm = TRUE)

# 2. Visualization --------------------------------------------------------
## 2.0 Number of images Landsat 5 ----------------------------------------------

n_images <- read.csv("trabajo/outputs/landsat_5.csv") %>% 
  group_by(Año) %>% 
  summarise(Num_imagenes = sum(Num_Imagenes))
n_images$date <- lubridate::year(n_images$Año)

n_images %>% 
  group_by(Mes) %>% 
  summarise(mes_ = mean(Num_Imagenes))

eventos <- c(1991, 2002)

ggplot(n_images, aes(x = Año, y = Num_imagenes)) +
  geom_col(fill = "steelblue") +
  geom_vline(xintercept = eventos, linetype = "dashed", color = c("red", "darkgreen"), alpha = 0.7) +
  annotate("text", x = 1991, y = 4, label = "Aoulouz Dam", 
           angle = 90, vjust = -0.5, size = 3) +
  annotate("text", x = 2002, y = 4, label = "Full system", 
           angle = 90, vjust = -0.5, size = 3) +
  labs(
    title = "Number of Images per year \nLandsat 5",
    x = "",
    y = ""
  ) +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("trabajo/outputs/plots/00.number_images.png")

## 2.01 Precipitation ----------------------------------------------


ggplot(precip_annual) +
  geom_col(data = precip_annual,
           aes(x = year, y = annual_total_precip_mm),
           fill = "lightblue", alpha = 0.3, width = 0.8,
           inherit.aes = FALSE) +
  geom_vline(xintercept = 1991, linetype = "dashed", color = "red", alpha = 0.7) +
  geom_vline(xintercept = 2002, linetype = "dashed", color = "darkgreen", alpha = 0.7) +
  # ANNOTATIONS
  annotate("text", x = 1991, y = 350, label = "Aoulouz Dam", 
           angle = 90, vjust = -0.5, size = 3) +
  annotate("text", x = 2002, y = 350, label = "Full system", 
           angle = 90, vjust = -0.5, size = 3) +
  labs(title = "Annual Precipitation",
       x = "Year",
       y = "Total mm") +
  theme_minimal() +
  theme(axis.title.y.right = element_text(color = "darkblue"))

ggsave("trabajo/outputs/plots/00.1.annual_precip.png")

## 2.1 Three periods - line ------------------------------------------------

gradient_3periods <- df_analysis %>%
  group_by(detailed_period, distance_km = round(distance_km, 1)) %>%
  summarise(
    ndvi_mean = mean(ndvi_mediana, na.rm = TRUE),
    ndvi_se = sd(ndvi_mediana, na.rm = TRUE) / sqrt(n()),
    n_obs = n(),
    .groups = 'drop'
  ) %>%
  filter(distance_km <= 16) # Your buffer is 16km

p_3periods <- ggplot(gradient_3periods,
                     aes(x = distance_km, 
                         y = ndvi_mean, color = detailed_period)) +
  geom_line(size = 1.2) +
  geom_ribbon(aes(ymin = ndvi_mean - ndvi_se, ymax = ndvi_mean + ndvi_se, 
                  fill = detailed_period), alpha = 0.2, color = NA) +
  geom_vline(xintercept = c(2, 5, 10), linetype = "dashed", alpha = 0.5) +
  scale_color_manual(values = c("red", "orange", "darkgreen")) +
  scale_fill_manual(values = c("red", "orange", "darkgreen")) +
  labs(title = "Dam Effects: Three Key Periods",
       subtitle = "90-00: Insufficient dam | 2002+: Second dam",
       x = "Distance to dam (km)", y = "Average NDVI",
       color = "Period", fill = "Period") +
  theme_minimal() +
  xlim(0, 16)+
  geom_col(data = precip_annual,
           aes(x = year, y = annual_total_precip_mm * scale_factor),
           fill = "lightblue", alpha = 0.3, width = 0.8,
           inherit.aes = FALSE)

print(p_3periods)

ggsave("trabajo/outputs/plots/01.p_3periods.png")


## 2.2 Temporal trend ------------------------------------------------------


zones_time <- df_analysis %>%
  group_by(distance_zone, year) %>%
  summarise(
    ndvi_mean = mean(ndvi_mediana, na.rm = TRUE),
    ndvi_se = sd(ndvi_mediana, na.rm = TRUE) / sqrt(n()),
    .groups = 'drop'
  ) 

p_temporal <- ggplot(zones_time,
                     aes(x = year, y = ndvi_mean, color = distance_zone)) +
  geom_line(size = 1.2) +
  geom_ribbon(aes(ymin = ndvi_mean - ndvi_se, 
                  ymax = ndvi_mean + ndvi_se, 
                  fill = distance_zone), alpha = 0.2, color = NA) +
  # KEY EVENT LINES
  geom_vline(xintercept = 1991, linetype = "dashed", color = "red", alpha = 0.7) +
  geom_vline(xintercept = 2002, linetype = "dashed", color = "darkgreen", alpha = 0.7) +
  # ANNOTATE EVENTS
  annotate("text", x = 1991, y = 0.20, label = "Aoulouz Dam", 
           angle = 90, vjust = -0.5, size = 3) +
  annotate("text", x = 2002, y = 0.20, label = "Full system", 
           angle = 90, vjust = -0.5, size = 3) +
  scale_color_viridis_d(name = "Zone") +
  scale_fill_viridis_d(name = "Zone") +
  labs(title = "NDVI Temporal Evolution by Distance Zones",
       subtitle = "Confidence band = standard error",
       x = "Year", y = "Average NDVI") +
  theme_minimal()

print(p_temporal)
ggsave("trabajo/outputs/plots/02.p_temporal.png")




##########################################



ggplot(zones_time,
                     aes(x = year, y = ndvi_mean, color = distance_zone)) +
  # PRECIPITACIÓN - como barras en el fondo
  geom_col(data = precip_annual,
           aes(x = year, y = annual_total_precip_mm * scale_factor),
           fill = "lightblue", alpha = 0.3, width = 0.8,
           inherit.aes = FALSE) +
  # NDVI - líneas originales
  geom_line(size = 1.2) +
  geom_ribbon(aes(ymin = ndvi_mean - ndvi_se, 
                  ymax = ndvi_mean + ndvi_se, 
                  fill = distance_zone), alpha = 0.2, color = NA) +
  # EVENT LINES
  geom_vline(xintercept = 1991, linetype = "dashed", color = "red", alpha = 0.7) +
  geom_vline(xintercept = 2002, linetype = "dashed", color = "darkgreen", alpha = 0.7) +
  # ANNOTATIONS
  annotate("text", x = 1991, y = 0.20, label = "Aoulouz Dam", 
           angle = 90, vjust = -0.5, size = 3) +
  annotate("text", x = 2002, y = 0.20, label = "Full system", 
           angle = 90, vjust = -0.5, size = 3) +
  # ESCALAS
  scale_color_viridis_d(name = "Zone") +
  scale_fill_viridis_d(name = "Zone") +
  scale_y_continuous(
    name = "Average NDVI",
    sec.axis = sec_axis(~ . / scale_factor, 
                        name = "Annual Precipitation (mm)")
  ) +
  labs(title = "NDVI Temporal Evolution by Distance Zones with Annual Precipitation",
       subtitle = "Confidence band = standard error; Blue bars = precipitation",
       x = "Year") +
  theme_minimal() +
  theme(axis.title.y.right = element_text(color = "darkblue"))

ggsave("trabajo/outputs/plots/02.p_temporal_rain.png")


## 2.3 Direction Analysis --------------------------------------------------

p_direction_map <- ggplot() +
  geom_sf(data = filter(df_analysis, year == 2000), 
          aes(color = direction, geometry = geometry), 
          size = 0.3, alpha = 0.8) +
  geom_sf(data = dam_aoulouz_utm, 
          aes(geometry = geom),
          color = "black", 
          fill = "white", 
          size = 3, 
          shape = 23) +  # Diamond shape for dam
  scale_color_manual(
    name = "Direction from Dam",
    values = c("North" = "#2E8B57",    
               "South" = "#CD853F",    
               "East" = "#4682B4",       
               "West" = "#DC143C"),    
    guide = guide_legend(override.aes = list(size = 2, 
                                             alpha = 1))
  ) +
  # Professional styling
  labs(
    title = "Direction from Aoulouz Dam",
    caption = "Each pixel represents 20m × 20m area"
  ) +
  
  # Clean theme
  theme_void() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray30"),
    plot.caption = element_text(size = 9, color = "gray50", hjust = 1),
    legend.position = "right",
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 10),
    legend.key.size = unit(0.8, "cm"),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(10, 10, 10, 10)
  ) 

print(p_direction_map)
ggsave("trabajo/outputs/plots/03.direction_ex.png")

## 2.4 Gradient ------------------------------------------------------------

directional_gradient <- df_analysis %>%
  group_by(direction, detailed_period, distance_km = round(distance_km, 1)) %>%
  summarise(ndvi_mean = mean(ndvi_mediana, na.rm = TRUE), .groups = 'drop') %>%
  filter(distance_km <= 16)

p_directional <- ggplot(directional_gradient, aes(x = distance_km, y = ndvi_mean, color = detailed_period)) +
  geom_line(size = 1) +
  facet_wrap(~direction, ncol = 2) +
  geom_vline(xintercept = c(2, 5, 10), linetype = "dashed", alpha = 0.3) +
  scale_color_manual(values = c("red", "orange", "darkgreen")) +
  labs(title = "NDVI Gradient by Direction and Period",
       subtitle = "Is the effect stronger towards North/East where the dam is located?",
       x = "Distance (km)", y = "Average NDVI") +
  theme_minimal()

print(p_directional)
ggsave("trabajo/outputs/plots/04.p_directional.png")


## 2.5 Comparative maps by period ------------------------------------------

multiple_changes <- df_analysis %>%
  group_by(id) %>%
  summarise(
    coord_x = first(coord_x),
    coord_y = first(coord_y),
    distance_km = first(distance_km),
    # Period averages 
    ndvi_pre = mean(ndvi_mediana[year < 1991], na.rm = TRUE),
    ndvi_aoulouz = mean(ndvi_mediana[year >= 1991 & year <= 2002], na.rm = TRUE),
    ndvi_system = mean(ndvi_mediana[year > 2002], na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  mutate(
    change_aoulouz = ndvi_aoulouz - ndvi_pre,
    change_system = ndvi_system - ndvi_pre,
    system_improvement = ndvi_system - ndvi_aoulouz
  ) %>%
  pivot_longer(
    cols = c(change_aoulouz, change_system, system_improvement),
    names_to = "change_type",
    values_to = "change"
  ) %>%
  mutate(
    change_type = case_when(
      change_type == "change_aoulouz" ~ "Aoulouz only\n(1991-02 vs Pre)",
      change_type == "change_system" ~ "Full system\n(2003-10 vs Pre)", 
      change_type == "system_improvement" ~ "System benefit\n(2003-10 vs 1991-02)"
    )
  ) %>% 
  mutate(
    transition = factor(change_type, levels = c(
      "Aoulouz only\n(1991-02 vs Pre)",
      "System benefit\n(2003-10 vs 1991-02)", 
      "Full system\n(2003-10 vs Pre)"
    )))


# Convert to sf
changes_sf_multiple <- multiple_changes %>%
  filter(!is.na(coord_x), !is.na(coord_y), !is.na(change)) %>%
  st_as_sf(coords = c("coord_x", "coord_y"), crs = st_crs(df))

# Map with facets
p_facet_maps <- ggplot(changes_sf_multiple) +
  geom_sf(aes(color = change), size = 0.8) +
  facet_wrap(~change_type, ncol = 3) +
  scale_color_gradient2(
    low = "red", mid = "white", high = "darkgreen", 
    midpoint = 0, name = "NDVI\nChange",
    limits = c(-0.05, 0.05)  # Adjust according to your data
  ) +
  labs(title = "Effect Comparison by Periods",
       subtitle = "Red = deterioration, Green = improvement") +
  theme_void() +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    legend.position = "bottom"
  )

print(p_facet_maps)
ggsave("trabajo/outputs/plots/05.p_facet_maps.png")



## 2.6. 4 year window ------------------------------------------------------

detailed_changes <- df_analysis %>%
  group_by(id) %>%
  summarise(
    coord_x = first(coord_x),
    coord_y = first(coord_y),
    ndvi_pre_early = mean(ndvi_mediana[year >= 1984 & year <= 1986], na.rm = TRUE),  # Early pre
    ndvi_pre_late = mean(ndvi_mediana[year >= 1987 & year <= 1990], na.rm = TRUE),   # Late pre
    ndvi_aou_early = mean(ndvi_mediana[year >= 1991 & year <= 1994], na.rm = TRUE),
    ndvi_aou_mid = mean(ndvi_mediana[year >= 1995 & year <= 1998], na.rm = TRUE),
    ndvi_aou_late = mean(ndvi_mediana[year >= 1999 & year <= 2002], na.rm = TRUE),
    ndvi_sys_early = mean(ndvi_mediana[year >= 2003 & year <= 2006], na.rm = TRUE),
    ndvi_sys_late = mean(ndvi_mediana[year >= 2007 & year <= 2010], na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  mutate(
    step_0 = ndvi_pre_late - ndvi_pre_early,     # Early pre → Late pre (natural evolution)
    step_1 = ndvi_aou_early - ndvi_pre_late,     # Late pre → Initial Aoulouz
    step_2 = ndvi_aou_mid - ndvi_aou_early,      # Initial → Mid Aoulouz
    step_3 = ndvi_aou_late - ndvi_aou_mid,       # Mid → Late Aoulouz  
    step_4 = ndvi_sys_early - ndvi_aou_late,     # Late Aoulouz → Initial system
    step_5 = ndvi_sys_late - ndvi_sys_early      # Initial → Late system
  ) %>%
  pivot_longer(
    cols = starts_with("step_"),
    names_to = "transition",
    values_to = "change"
  ) %>%
  mutate(
    transition = case_when(
      transition == "step_0" ~ "Natural evolution\n(1984-86 → 1987-90)", 
      transition == "step_1" ~ "Pre → Initial Aoulouz\n(1987-90 → 1991-94)", 
      transition == "step_2" ~ "Initial → Mid Aoulouz\n(1991-94 → 1995-98)", 
      transition == "step_3" ~ "Mid → Late Aoulouz\n(1995-98 → 1999-02)", 
      transition == "step_4" ~ "Aoulouz → System\n(1999-02 → 2003-06)", 
      transition == "step_5" ~ "Initial → Late system\n(2003-06 → 2007-10)" 
    )) %>% 
  mutate(
    transition = factor(transition, levels = c(
      "Natural evolution\n(1984-86 → 1987-90)", 
      "Pre → Initial Aoulouz\n(1987-90 → 1991-94)",
      "Initial → Mid Aoulouz\n(1991-94 → 1995-98)",
      "Mid → Late Aoulouz\n(1995-98 → 1999-02)", 
      "Aoulouz → System\n(1999-02 → 2003-06)", 
      "Initial → Late system\n(2003-06 → 2007-10)" 
    )))


# Convert to sf and map
detailed_changes_sf <- detailed_changes %>%
  filter(!is.na(coord_x), !is.na(coord_y), !is.na(change)) %>%
  st_as_sf(coords = c("coord_x", "coord_y"), crs = st_crs(df))

# Map with more periods
p_detailed_maps <- ggplot(detailed_changes_sf) +
  geom_sf(aes(color = change), size = 0.6) +
  facet_wrap(~transition, ncol = 3) +
  scale_color_gradient2(
    low = "red", mid = "white", high = "darkgreen", 
    midpoint = 0, name = "NDVI\nChange"
  ) +
  labs(title = "Step-by-Step Evolution: Changes Between Consecutive Periods",
       subtitle = "Red = deterioration, Green = improvement") +
  theme_void() +
  theme(
    strip.text = element_text(size = 9, face = "bold"),  # Slightly smaller text for 6 maps
    legend.position = "bottom"
  )

ggsave("trabajo/outputs/plots/06.p_detailed_maps.png")