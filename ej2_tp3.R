


library(readr)

file<-"https://raw.githubusercontent.com/gustavodluna/mineria_datos_tp3/refs/heads/main/orders.csv"

pedidos<- read.csv(
  file = file,
  header = TRUE,
  sep = ";"
)


# Inspeccionar las primeras filas del archivo
head(pedidos)

# Verificar las columnas
str(pedidos)

# Filtrar productos clave en las transacciones (ajustar con productos de tu dataset)
productos_relevantes <- c("Banana", "Sparkling Water Grapefruit", "Blueberry Whole Milk Yogurt Pouch")
productos_relevantes
# Crear una tabla binaria: una fila por pedido, columnas indicando si el producto fue comprado
transacciones_binarias <- as.data.frame.matrix(table(pedidos$order_id, pedidos$product_name))
head(transacciones_binarias)

# Filtrar solo los productos relevantes
transacciones_binarias <- transacciones_binarias[, productos_relevantes]

# Asegurar que son variables binarias (0 o 1)
transacciones_binarias <- ifelse(transacciones_binarias > 0, 1, 0)
head(transacciones_binarias)

# Cargar el paquete
library(bnlearn)

# Crear una red bayesiana vacía con los nodos de los productos
nodos <- c("Banana", "Sparkling Water Grapefruit", "Blueberry Whole Milk Yogurt Pouch")
red <- empty.graph(nodes = nodos)

# Definir los arcos entre los nodos (Banana afecta Sparkling Water Grapefruit, que a su vez afecta Yogurt)
arcs(red) <- matrix(c("Banana", "Sparkling Water Grapefruit",
                      "Sparkling Water Grapefruit", "Blueberry Whole Milk Yogurt Pouch"), 
                    byrow = TRUE, ncol = 2)

# Visualizar la estructura de la red
print(red)

remove.packages("bnlearn")  # Elimina el paquete existente
