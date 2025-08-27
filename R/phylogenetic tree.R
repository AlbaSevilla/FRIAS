library("treeio")
library("ggtree")

nwk <- system.file("extdata", "sample.nwk", package="treeio")
tree <- read.tree(nwk)

ggplot(tree, aes(x, y)) + geom_tree() + theme_tree()
ggtree(tree, color="firebrick", size=2, linetype="dotted")
ggtree(tree, ladderize=FALSE)
ggtree(tree, branch.length="none")

library(ggtree)
set.seed(2017-02-16)
tree <- rtree(50)
# ggtree(tree)
# ggtree(tree, layout="roundrect")
# ggtree(tree, layout="slanted")
# ggtree(tree, layout="ellipse")
# ggtree(tree, layout="circular")
# ggtree(tree, layout="fan", open.angle=120)
# ggtree(tree, layout="equal_angle")
# ggtree(tree, layout="daylight")
# ggtree(tree, branch.length='none')
# ggtree(tree, layout="ellipse", branch.length="none")

#El circular que quiero
ggtree(tree, branch.length='none', layout='circular')

# ggtree(tree, layout="daylight", branch.length = 'none')




############################
library(ggtree)
library(data.tree)
library(dplyr)
library(readr)
library(tibble)
library(tidytree)


# Reemplaza con la ruta a tu archivo CSV real
masterlist <- read_excel("OutputFiles/Intermediate/Step10_CorrectedAcceptedNameGBIF_Masterlist.xlsx")
masterlist <- masterlist[c(1:200),]

masterlist$Kingdom <- sub(",.*", "", masterlist$Kingdom)
masterlist$Phylum <- sub(",.*", "", masterlist$Phylum)
masterlist$Class <- sub(",.*", "", masterlist$Class)
masterlist$Order <- sub(",.*", "", masterlist$Order)
masterlist$Family <- sub(",.*", "", masterlist$Family)
masterlist$AcceptedNameGBIF <- sub(",.*", "", masterlist$AcceptedNameGBIF)


# Crear path jerárquico para cada especie (igual, incluyendo Class)
masterlist$pathString <- apply(masterlist[, c("Kingdom", "Phylum", "Class", "Order", "Family", "AcceptedNameGBIF")],
                               1, function(x) paste(c("Life", x), collapse = "/"))

# Crear árbol jerárquico
tax_tree <- as.Node(masterlist)
phylo_tree <- as.phylo.Node(tax_tree)

# Dibujar árbol circular SIN etiquetas
p <- ggtree(phylo_tree, layout = "circular")

tip_data <- p$data %>%
  filter(isTip) %>%
  mutate(label = sub(".*/", "", label),
         label = gsub("_", " ", label)) %>%
  left_join(masterlist, by = c("label" = "AcceptedNameGBIF"))

# Dibujar sin etiquetas y colorear solo los puntos finales por Class
p +
  geom_tippoint(data = tip_data, aes(x = x, y = y, color = Kingdom), size = 2) +
  theme(legend.position = "right") +
  ggtitle("Árbol jerárquico coloreado por Kingdom") +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8)
  )


