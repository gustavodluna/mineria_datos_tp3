

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

# Distribución del tamanho de las transacciones por cuantiles

summary(tamanhos_trans)

# Frecuencia de cada item

head(itemFrequency(transacciones))

# Mantenemos las transacciones con al menos dos items

transacciones <- transacciones[tamanhos_trans>1]
dim(transacciones)

# Umbral de soporte

soporte<- 15/dim(transacciones)[1]
soporte

# Umbral de confianza

confianza <- 0.7

# Busqueda de itemsets frecuentes

itemsets_frecuentes <- apriori(
                              data = transacciones,
                              parameter = list(support=soporte,
                                               target="frequent itemsets"
                                               ),
                              control=list(verbose=FALSE)
                              )
summary(itemsets_frecuentes)

# Top itemsets mas frecuentes

top6_itemsets <- sort(itemsets_frecuentes,by="support",decreasing = TRUE)[1:6]

inspect(top6_itemsets)

library(dplyr)

as(top6_itemsets,Class = "data.frame") %>%
  ggplot(aes(x=reorder(items,support),y=support)) +
  geom_col(fill="skyblue3",width = 0.5) +
  coord_flip() +
  labs(y="soporte",x="itemsets") +
  theme_bw()

# Itemsets frecuentes que contienen "Banana"

itemsets_banana <- arules::subset(itemsets_frecuentes,
                                  subset=items %ain% "Banana"
                                  )
inspect(itemsets_banana[1:5])

# Obtención de reglas de asociación 

reglas <- apriori(
                  data=transacciones,
                  parameter = list(support=soporte,
                                   confidence=confianza,
                                   target="rules"
                                   ),
                  control=list(verbose=FALSE)
                  )
print(paste("Reglas generadas: ", length(reglas)))

summary(reglas)

# Reglas obtenidas ordenadas por orden descendente de confianza

inspect(sort(reglas,decreasing = TRUE,by="confidence"))


reglas_filtradas<- subset(
                          reglas,
                          subset=lhs %ain% "Blueberry Whole Milk Yogurt Pouch" &
                            confidence>0.9
                          )
inspect(reglas_filtradas)

reglas_maximales<-reglas[is.maximal(reglas)]
reglas_maximales

inspect(reglas_maximales)

# Reglas con items relacionados con Sparkling Water Grapefruit

reglas_waterGrape<- apriori(
                            transacciones,
                            parameter = list(support=soporte,
                                             confidence=confianza,
                                             target="rules"
                                             ),
                            appearance = list(
                                              rhs="Sparkling Water Grapefruit",
                                              default="lhs"
                                              ),
                            control = list(verbose=FALSE)
                            )
inspect(reglas_waterGrape)


testFisher<- interestMeasure (
                              reglas,
                              measure = "fishersExactTest",
                              transactions = transacciones
                              )
summary(testFisher)

# Añadimos los indices al conjunto de reglas

quality(reglas)<- cbind(quality(reglas),testFisher)

library(arulesViz)

# Grafico de dispersión coloreado en función del lift

plot(
  reglas,
  measure=c("support","confidence"),
  shading="lift"
  )

# Grafico de disperión coloreado en función del número de items

plot(
  reglas,
  measure=c("support","confidence"),
  shading="order"
)

# Gráfico de coordenadas paralelas

plot(
    reglas,
    method = "paracoord"
    )
