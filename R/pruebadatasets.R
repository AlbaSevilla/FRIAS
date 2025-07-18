# Crear los datasets de ejemplo
dataset1 <- data.frame(
  especies = c("aaaa", "bbbb", "ccccc", "dddd", "eeee"),
  habitat_real = factor(c("FRESHWATER", "NO FRESHWATER", "FRESHWATER", "FRESHWATER", "NO FRESHWATER"))
)

dataset2 <- data.frame(
  especies = c("aaaa", "bbbb", "ccccc", "dddd", "eeee"),
  habitat_predicho = factor(c("FRESHWATER", "FRESHWATER", "FRESHWATER", "FRESHWATER", "NO FRESHWATER"))
)

# Unir los datasets por la columna 'especies'
dataset_completo <- merge(dataset1, dataset2, by = "especies")

# Ver la matriz de confusión
conf_matrix <- table(dataset_completo$habitat_real, dataset_completo$habitat_predicho)
print(conf_matrix)

# Calcular la precisión (accuracy)
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
print(paste("Precisión: ", round(accuracy * 100, 2), "%"))

# Calcular la precisión
precision <- conf_matrix[2,2] / sum(conf_matrix[,2])
# Calcular el recall
recall <- conf_matrix[2,2] / sum(conf_matrix[2,])
# Calcular el F1-Score
f1_score <- 2 * (precision * recall) / (precision + recall)
print(paste("F1-Score: ", round(f1_score, 2)))


# Matriz de confusión normalizada (por filas)
conf_matrix_normalized <- prop.table(conf_matrix, 1)
print(conf_matrix_normalized)
