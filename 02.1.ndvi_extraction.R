# EXTRACT NDVI FOR GRID 
library(terra)
library(sf)
library(dplyr)
library(ggplot2)
library(parallel)     # Para makeCluster(), stopCluster()
library(doParallel)   # Para registerDoParallel()  
library(foreach)      # Para %dopar%

rm(list = ls())


# 1. Data -------

# Grid
st_layers("trabajo/shape_squares.gpkg")
squares <- st_read("trabajo/shape_squares.shp")

# Path where NDVI rasters
ruta_ndvi <- "trabajo/NDVI_Results/"
#setwd(ruta_ndvi)

# Buscar archivos NDVI
archivos_ndvi <- list.files(path = ruta_ndvi,
                            pattern = "^NDVI_\\d{4}_\\d{2}\\.tif$", 
                            full.names = TRUE)


# Convert to SpatVector for terra
squares_vect <- vect(squares)

# 2. Function----

extraer_ndvi <- function(archivo_raster, grid_vector, campo_id = "id") {
  # Cargar raster
  ndvi <- rast(archivo_raster)
  # Extraer ambas estadísticas en una sola pasada
  stats <- extract(ndvi, grid_vector, fun = function(x) {
    if (all(is.na(x))) {
      return(c(NA, 0))  # mediana, conteo
    } else {
      valid_vals <- x[!is.na(x)]
      return(c(median(valid_vals), length(valid_vals)))
    }
  }, bind = T)
  
  # Acceder por nombre de columna 
  return(setNames(data.frame(
    stats[, campo_id], 
    stats[, "NDVI"],   # Primera estadística (mediana)
    stats[, "NDVI.1"]  # Segunda estadística (conteo)
  ),c("id", 
      "ndvi_mediana",
      "pixeles_validos")))
}


# 3. Pararel procesing -----

cat("Cores:", detectCores(), "\n")

# Use 6 cores 
n_cores <- min(6, detectCores() - 2)

# Configurar cluster
cl <- makeCluster(n_cores)
registerDoParallel(cl)

# Cargar terra en cada worker
clusterEvalQ(cl, {
  library(terra)
})

# Exportar objetos necesarios a workers
clusterExport(cl, c("squares", "extraer_ndvi"))

cat("Procesando", length(archivos_ndvi), "rasters en paralelo...\n")
tiempo_total_inicio <- Sys.time()

# Procesamiento paralelo
resultados <- foreach(i = seq_along(archivos_ndvi), 
                      .packages = c("terra"),
                      .combine = 'list',
                      .multicombine = TRUE) %dopar% {
                        
                        # CREAR squares_vect DENTRO del worker
                        squares_vect_local <- vect(squares)  # Crear localmente
                        
                        archivo <- archivos_ndvi[i]
                        nombre <- basename(archivo)
                        partes <- strsplit(gsub("\\.tif$", "", nombre), "_")[[1]]
                        
                        # Usar el objeto local
                        stats <- extraer_ndvi(archivo, squares_vect_local)  # Usar objeto local
                        stats$year <- as.numeric(partes[2])
                        stats$month <- as.numeric(partes[3])
                        
                        stats <- stats[, c("id", "year", "month", "ndvi_mediana", "pixeles_validos")]
                        return(stats)
                      }

# Cerrar cluster
stopCluster(cl)

tiempo_total_fin <- Sys.time()
tiempo_total <- round(as.numeric(difftime(tiempo_total_fin, tiempo_total_inicio, units = "mins")), 1)

cat("Procesamiento paralelo completado en", tiempo_total, "minutos\n")
cat("Promedio por raster:", round(tiempo_total/length(archivos_ndvi)*60, 1), "segundos\n")

# Combinar todos los resultados
datos_finales <- do.call(rbind, resultados)

rm(i, archivos_ndvi, n_cores, ruta_ndvi, tiempo_total,
   tiempo_total_fin, tiempo_total_inicio, cl)

save.image("trabajo/outputs/workspace_completo-2001_2010.RData")
write.csv(datos_finales, "trabajo/outputs/datos_finales-2001_2010.csv", row.names = FALSE)

# 4. Consolidate -----
load("trabajo/outputs/workspace_completo.RData")
# Limpiar memoria
rm(resultados)
gc()

#calculate centroids for each cell
centroides <- st_centroid(squares)

# dam
dam_aoulouz <- st_read("work/composition.gpkg", layer = "dam_Aoulouz") 
dam_aoulouz_utm <- st_transform(dam_aoulouz, st_crs(centroides))

centroides <- centroides %>%
  mutate(
    x_centroide = st_coordinates(.)[, "X"],
    y_centroide = st_coordinates(.)[, "Y"],
    distancia_represa = as.numeric(st_distance(., st_centroid(dam_aoulouz_utm)))
  ) %>% 
  select(id, distancia_represa)

datos_finales <- datos_finales %>%
  left_join(st_drop_geometry(centroides), by = "id")

write.csv(datos_finales, "trabajo/outputs/datos_finales.csv", row.names = FALSE)

# Unificar periodos


load("trabajo/outputs/workspace_completo.RData")
rm(resultados)
gc()
data_1984_2000 <- read.csv("trabajo/outputs/datos_finales-1984-2000.csv")
data_2000_2010 <- read.csv("trabajo/outputs/datos_finales-2001_2010.csv")
centroides <- st_centroid(squares)
# Cargar represa
dam_aoulouz <- st_read("work/composition.gpkg", layer = "dam_Aoulouz") 
dam_aoulouz_utm <- st_transform(dam_aoulouz, st_crs(centroides))

# Unir a datos_finales
data_2000_2010 <- data_2000_2010 %>%
  left_join(st_drop_geometry(centroides), by = "id")

datos_finales <- rbind(data_1984_2000, data_2000_2010)
write.csv(datos_finales, "trabajo/outputs/datos_finales.csv", row.names = FALSE)



