

library(readr)

file<-"https://raw.githubusercontent.com/gustavodluna/mineria_datos_tp3/refs/heads/main/orders.csv"

pedidos<- read.csv(
                    file = file,
                   header = TRUE,
                   sep = ";"
                   )

head(pedidos)

library(arules)

# Cargamos los datos a un objeto de tipo transacción

transacciones<- read.transactions(
                                  file=file,
                                  header=TRUE,
                                  format = "single",
                                  sep = ";",
                                  cols = c("order_id","product_name"),
                                  rm.duplicates = TRUE
                                  )
# El objeto contiene transacciones en filas e items en columnas

rownames(transacciones)[1:3]

colnames(transacciones)[1:3]

# Dimensiones

transacciones

# Inspección de los items en cada transacción

inspect(transacciones[1:2])


library(ggplot2)

# Tamanos de todas las transacciones

tamanhos_trans<- data.frame(tamanho=size(transacciones))

head(tamanhos_trans)

# Distribución del tamanho de todas las transacciones

ggplot(tamanhos_trans,aes(x=tamanho)) +
  geom_density(fill="orangered3") +
  labs(x="Tamaño de las transacciones") +
  theme_bw()
