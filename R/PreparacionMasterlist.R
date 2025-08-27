#PREPARAR CORRECTAMENTE LA MASTERLIST:

#1 ELIMINAR TODOS LOS ESPACIOS SIMPLES O DOBLES DESPUÉS DE UNA COMA O PUNTO Y COMA
Step2_MasterList <- read_excel("Output/Intermediate/Step2_MasterList.xlsx")
#Eliminar todos los espacios despues de , o ;
Step2_MasterList[] <- lapply(Step2_MasterList, function(x) {
  gsub("([;,])\\s+", "\\1", x)
})

View(Step2_MasterList)
unique(Step2_MasterList$Kingdom)

#2 CAMBIAR TODAS LAS COMAS O PUNTO Y COMA POR PUNTO Y COMA
Step2_MasterList[] <- lapply(Step2_MasterList, function(x) {
  gsub("[,;]", ";", x)
})

#3 ELIMINAR CADENAS DE CARACTERES DUPLICADAS DENTRO DE UNA MISMA CELDA PARA TODO EL DATASET
elimina_duplicados <- function(x) {
  sapply(strsplit(x, ";"), function(y) paste(unique(y), collapse = ";"))
}
Step2_MasterList[] <- lapply(Step2_MasterList, elimina_duplicados)

#4 ORDENAR POR ORDEN ALFABETICO LAS CADENAS DE CARACTERES DENTRO DE CADA CELDA
ordena_cadenas <- function(x) {
  sapply(strsplit(x, ";"), function(y) paste(sort(y), collapse = ";"))
}
Step2_MasterList[] <- lapply(Step2_MasterList, ordena_cadenas)
