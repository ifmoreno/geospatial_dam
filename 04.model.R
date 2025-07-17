#install.packages("modelsummary")
library(tidyverse)
library(fixest)
library(modelsummary)
library(tidyverse)
library(sf)
library(stargazer)

rm(list = ls())
gc()

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

# 1. Prepare data
df_reg <- df_analysis %>%
  sf::st_drop_geometry() %>% 
  filter(year < 1998)%>% 
  mutate(
    # Variables de tratamiento simples
    near_dam = ifelse(distance_km <= 8, 1, 0),
    post_dam = ifelse(year >= 1991, 1, 0),
    
    # Interacción simple
    near_post = near_dam * post_dam,
    
    # Interacción con precipitación
    precip_x_near = annual_total_precip_mm * near_dam,
    precip_x_post = annual_total_precip_mm * post_dam,
    precip_x_near_post = annual_total_precip_mm * near_dam * post_dam,
    
    # Distancia continua para robustez
    dist_x_post = distance_km * post_dam,
    
    # Categorías de precipitación 
    precip_tercile = ntile(annual_total_precip_mm, 3),
    dry_year = ifelse(precip_tercile == 1, 1, 0),
    wet_year = ifelse(precip_tercile == 3, 1, 0)
  )

df_reg_na <- df_reg %>% 
  group_by(year, month) %>% 
  summarise(total_pixels = n(),
            missing_ndvi = sum(is.na(ndvi_mediana)),
            pct_missing = 100 * missing_ndvi / total_pixels) %>% 
  filter(missing_ndvi>0)

missing_pattern <- df_reg %>% 
  group_by(year, month) %>% 
  summarise(
    total_pixels = n(),
    missing_ndvi = sum(is.na(ndvi_mediana)),
    pct_missing = 100 * missing_ndvi / total_pixels,
    .groups = 'drop'
  )


# MODEL 1: Basic DiD
m1 <- feols(ndvi_mediana ~ near_post | 
              id + year + month, 
            data = df_reg, 
            cluster = "id")

# MODEL 2: Heterogeneity by Direction
m2 <- feols(ndvi_mediana ~ near_post + 
              i(direction, near_post, ref = "North") | 
              id + year + month, 
            data = df_reg, 
            cluster = "id")

# MODEL 4: Triple Interaction with Precipitation
m4 <- feols(ndvi_mediana ~ near_post + 
              precip_x_near + 
              precip_x_near_post | 
              id + year + month, 
            data = df_reg, 
            cluster = "id")

# MODEL 6: Direction + Precipitation (Full Model)
m6 <- feols(ndvi_mediana ~ near_post + 
              i(direction, near_post, ref = "North") +
              precip_x_near + 
              precip_x_near_post | 
              id + year + month, 
            data = df_reg, 
            cluster = "id")

#  RESULTS TABLE

models <- list(
  "(1) Basic" = m1,
  "(2) Direction" = m2,
  "(3) Precip." = m4,
  "(4) Full" = m6
)

# Create table
modelsummary(
  models, 
  output = "trabajo/outputs/table_reg.docx",
  stars = c('*' = .1, '**' = .05, '***' = .01),
  coef_rename = c(
    "near_post" = "Near × Post",
    "direction::East:near_post" = "East × Near × Post",
    "direction::South:near_post" = "South × Near × Post",
    "direction::West:near_post" = "West × Near × Post",
    "precip_x_near" = "Precipitation × Near",
    "precip_x_near_post" = "Precipitation × Near × Post"
  ),
  gof_omit = 'AIC|BIC|Log.Lik|RMSE|FE:',
  gof_map = c("nobs", "r.squared", "adj.r.squared"),
  title = "Impact of Aoulouz Dam on NDVI",
  notes = c(
    "Standard errors clustered at pixel level. All models include pixel, year, and month fixed effects.",
    "Period: 1984-1997. Treatment: distance ≤ 8km from dam."
  )
)


# Descriptive --------------------------------------------------------------

# Calcular estadísticas para cada grupo
stats_summary <- df_reg %>%
  select(ndvi_mediana, annual_total_precip_mm, near_dam) %>%
  na.omit() %>%
  group_by(near_dam) %>%
  summarise(
    ndvi_mean = round(mean(ndvi_mediana), 3),
    ndvi_sd = round(sd(ndvi_mediana), 3),
    precip_mean = round(mean(annual_total_precip_mm), 1),
    precip_sd = round(sd(annual_total_precip_mm), 1),
    n = n(),
    .groups = "drop"
  ) %>%
  mutate(near_dam = as.character(near_dam))  # Convertir a character

# Calcular para muestra completa
full_stats <- df_reg %>%
  select(ndvi_mediana, annual_total_precip_mm) %>%
  na.omit() %>%
  summarise(
    near_dam = "Full",
    ndvi_mean = round(mean(ndvi_mediana), 3),
    ndvi_sd = round(sd(ndvi_mediana), 3),
    precip_mean = round(mean(annual_total_precip_mm), 1),
    precip_sd = round(sd(annual_total_precip_mm), 1),
    n = n()
  )

all_stats <- bind_rows(full_stats, stats_summary)

# Crear matriz para stargazer
table_matrix <- matrix(c(
  all_stats$ndvi_mean[1], all_stats$ndvi_sd[1], all_stats$ndvi_mean[2], all_stats$ndvi_sd[2], all_stats$ndvi_mean[3], all_stats$ndvi_sd[3],
  all_stats$precip_mean[1], all_stats$precip_sd[1], all_stats$precip_mean[2], all_stats$precip_sd[2], all_stats$precip_mean[3], all_stats$precip_sd[3]
), nrow = 2, byrow = TRUE)

rownames(table_matrix) <- c("NDVI", "Precipitation (mm)")
colnames(table_matrix) <- c("Mean", "SD", "Mean", "SD", "Mean", "SD")

# Crear tabla con stargazer
stargazer(table_matrix,
          #type = "text",  # cambia a "latex" para LaTeX
          type = "latex",
          title = "Summary Statistics",
          header = FALSE,
          digits = 3)


# Missing -------------------------------------------------------

years <- min(df_reg$year):max(df_reg$year)
months <- 5:7
complete_grid <- expand_grid(year = years, month = months)

availability <- df_reg %>%
  group_by(year, month) %>%
  summarise(n_observations = n(), .groups = "drop")

image_availability <- complete_grid %>%
  left_join(availability, by = c("year", "month")) %>%
  mutate(
    has_image = !is.na(n_observations),
    n_observations = replace_na(n_observations, 0)
  )

# Graph
ggplot(image_availability, aes(x = factor(month), y = factor(year))) +
  geom_tile(aes(fill = has_image), color = "white", size = 0.1) +
  scale_fill_manual(
    values = c("TRUE" = "#52C4A0", "FALSE" = "#E74C3C"),
    labels = c("TRUE" = "True", "FALSE" = "False"),
    name = "Has Image"
  ) +
  scale_x_discrete(labels = month.abb) +
  labs(
    title = "Figure A2: Availability of Imagery Over Time",
    x = "Month",
    y = "Year"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 0),
    panel.grid = element_blank(),
    legend.position = "right"
  )
