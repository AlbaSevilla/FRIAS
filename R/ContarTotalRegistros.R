library(parallel)
library(tidyverse)
library(openxlsx)  # Asegúrate de cargar el paquete correcto para read.xlsx

# Detectar núcleos disponibles y crear un clúster
#num_cores <- detectCores()
#cl <- makeCluster(num_cores - 2)

# Leer archivo de entrada
ruta_inputfiles <- file.path("Inputfiles")
data_name_reptiles0 <- list.files(path = ruta_inputfiles,
                                  pattern = "Step0_OriginalDatabaseFreshwaterNODUPLICATES_GAVIA.xlsx", full.names = TRUE)

dat0 <- read.xlsx(data_name_reptiles0)
# Dividir el data frame por scientificName
df_list <- split(dat0, dat0$Binomial)

# Exportar variables y funciones necesarias al clúster
clusterExport(cl, varlist = c("df_list"), envir = environment())

# Definir función de limpieza
# limpiar_duplicados <- function(df) {
#   res <- sapply(df, function(col) {
#     texto <- as.character(col)
#     vals <- unlist(strsplit(texto, "[,;|]+|\\s{2,}"))
#     vals <- trimws(vals)
#     vals <- vals[vals != ""]
#     vals <- unique(vals)
#     paste(vals, collapse = ", ")
#   })
#   as.data.frame(t(res), stringsAsFactors = FALSE)
# }

# Exportar función al clúster
#clusterExport(cl, varlist = c("limpiar_duplicados"))

# Aplicar la función en paralelo
resultado_paralelo <- parLapply(cl, df_list, limpiar_duplicados)

# Combinar los resultados
amfibios_sin_duplicados <- do.call(rbind, resultado_paralelo)
rownames(amfibios_sin_duplicados) <- NULL

# Verificar número de filas resultantes
nrow(amfibios_sin_duplicados)

# Cerrar el clúster
#stopCluster(cl)

