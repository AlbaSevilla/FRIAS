# Cargar el paquete rgbif
library(rgbif)

# Crear un vector con los nombres de las especies
species_list <- c(
  "Actinocyclus normanii", "Ambiphrya ameiuri", "Ammannia verticillata", "Anser canadensis",
  "Argentious carpio", "Artemia tonosa", "Aulonocara", "Biomphalaria", "Cactaceae",
  "Cephaloidophora mucronata", "Cephaloidophora similis", "Channa", "Chenopodium ambrosioides",
  "Cichlidogyrus", "Clarias", "Corbicula", "Corbicula squalida", "Cordylophora", "Cuora",
  "Cyperus involucratus", "Cyperus ovularis", "Cyrtogramme melagynalis", "Echinochloa crusgalli",
  "Elamenopsis kempi", "Elodea canadensis", "Enterogyrus", "Ferrissia shimeki", "Fossaria viridis",
  "Gila", "Gobiidae", "Gyrodactylus", "Helisoma nigricans", "Helisoma tumida", "Hemicypris barbadensis",
  "Heterandria", "Hypanis glabra", "IPNV", "Kyllinga brevifolia", "Kyllinga bulbosa", "Kyllinga pumila",
  "Kyllingia monocephala", "Laonome armata", "Lythrum hyssopifolium", "Menidia", "Metynnis",
  "Nymphaea caerulea", "Oreochromis", "Oscillatoria", "Pachychilus", "Peridinium",
  "Physastra proteus", "Pila", "Pila luzonica", "Poeciliopsis monacha-occidentalis", "Polygonum hydropiper",
  "Pomacea gigas", "Procambius clarkii", "Pycreus mundtii", "Rana nigromaculata", "Scoparia dulcis",
  "Serrasalmidae", "Stenocuma cercaroides", "Stenocuma graciloides", "Stenocypris macedonica", "Termitidae",
  "Tetraonchus", "Tilapia", "Trichogaster fasciata", "Triodanis", "Tropicorbis orbiculus", "Uradiophora ramosa",
  "Veronica aquatica"
)

# Inicializar resultados y especies no encontradas
results <- list()
not_found_species <- c()

# Iterar sobre la lista de especies
for (species in species_list) {
  result <- name_backbone_checklist(species)

  # Verificar si se encontraron datos
  if (length(result$data) == 0) {
    not_found_species <- c(not_found_species, species)
  } else {
    results[[species]] <- result$data  # Almacenar datos si se encuentran
  }
  print(result[,c(3,4)])
}

# Mostrar resultados
if (length(not_found_species) > 0) {
  cat("No se han encontrado las siguientes especies:", paste(not_found_species, collapse = ", "), "\n")
} else {
  cat("Todas las especies se han encontrado.\n")
}

View(name_suggest("Cyrtogramme melagynalis")[["data"]])


View(name_backbone_checklist("Actinocyclus normanii (Gregory) Hustedt, 1957"))
library(taxize)
?taxize

library(taxize)

id <- get_gbifid(sci = "Ammannia verticillata", messages = FALSE)
name_backbone_checklist("Ammannia baccifera")$canonicalName

# Lista de nombres
names_list <- c(
  "Actinocyclus normanii", "Ambiphrya ameiuri", "Ammannia verticillata", "Anser canadensis",
  "Argentious carpio", "Artemia tonosa", "Aulonocara", "Biomphalaria", "Cactaceae",
  "Cephaloidophora mucronata", "Cephaloidophora similis", "Channa", "Chenopodium ambrosioides",
  "Cherax", "Cherax pulverulentus", "Cichlidogyrus", "Clarias", "Corbicula", "Corbicula squalida",
  "Cordylophora", "Cuora", "Cyperus involucratus", "Cyperus ovularis", "Cyrtogramme melagynalis",
  "Echinochloa crusgalli", "Elamenopsis kempi", "Elodea canadensis", "Enterogyrus",
  "Ferrissia shimeki", "Fossaria viridis", "Gila", "Gobiidae", "Gyrodactylus", "Helisoma nigricans",
  "Helisoma tumida", "Hemicypris barbadensis", "Heterandria", "Hypanis glabra", "IPNV",
  "Kyllinga brevifolia", "Kyllinga bulbosa", "Kyllinga pumila", "Kyllingia monocephala",
  "Lythrum hyssopifolium", "Menidia", "Metynnis", "Myxobolus artus", "Nymphaea caerulea",
  "Oreochromis", "Oscillatoria", "Pachychilus", "Pelophylax grafi", "Peridinium",
  "Physastra proteus", "Pila", "Pila luzonica", "Polygonum hydropiper", "Pomacea gigas",
  "Procambius clarkii", "Pycreus mundtii", "Radix javanica", "Rana nigromaculata",
  "Scoparia dulcis", "Serrasalmidae", "Stenocuma cercaroides", "Stenocuma graciloides",
  "Stenocypris macedonica", "Termitidae", "Tetraonchus", "Tilapia", "Trichogaster fasciata",
  "Triodanis", "Tropicorbis orbiculus", "Uradiophora ramosa", "Veronica aquatica"
)

# Aplicar name_backbone_checklist a cada nombre y extraer el rank
ranks <- sapply(names_list, function(name) {
  result <- name_backbone_checklist(name)
  result$rank
})

# Mostrar los resultados
ranks
