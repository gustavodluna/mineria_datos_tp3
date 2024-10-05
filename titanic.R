# Instalar los paquetes necesarios si no están instalados

install.packages("bnlearn")  # Para aprender y ajustar redes bayesianas
install.packages("tidyverse")  # Para manipulación de datos

# Cargar las librerías
library(bnlearn)
library(tidyverse)

# Cargar el dataset Titanic disponible en R en formato dataframe
# El dataset Titanic contiene información sobre la clase, sexo, edad y supervivencia de los pasajeros
data <- as.data.frame(Titanic)

# Expandir el dataframe para que cada fila represente un pasajero individual
# El dataset original tiene una columna Freq que indica cuántos pasajeros tienen ciertas características
titanic_df <- as.data.frame(xtabs(Freq ~ ., data = data))

# Ver la estructura del dataset
# Esto nos muestra las columnas y tipos de datos, importante para verificar el formato correcto
str(titanic_df)



# Paso 1: Aprender la estructura de la red bayesiana
# Usamos el algoritmo Hill-Climbing para aprender la estructura de la red a partir de los datos
dag <- hc(titanic_df)

# Visualizar la estructura de la red aprendida
# Este gráfico muestra cómo están relacionadas las variables del dataset

# Graficar la estructura de la red bayesiana
plot(dag, main = "Estructura de la Red Bayesiana del Titanic")

# Paso 2: Ajustar el modelo con probabilidades condicionales
# Ajustamos el modelo bayesiano a los datos discretizados
# Esto calcula las probabilidades condicionales entre las variables
fitted_bn <- bn.fit(dag, data = titanic_df)

# Paso 3: Realizar inferencias usando bnlearn directamente

# Calcular la probabilidad de que alguien sobreviva siendo un hombre de tercera clase

# Establecemos las condiciones: Hombre y de tercera clase
prob_survival <- cpquery(fitted_bn, 
                         event = (Survived == "Yes"), 
                         evidence = (Class == "3rd" & Sex == "Male"))

# Mostrar el resultado
print(prob_survival)

# [1] 0.4952077




# Probabilidad de sobrevivir siendo una mujer de primera clase
prob_survival_female_1st <- cpquery(fitted_bn, 
                                    event = (Survived == "Yes"), 
                                    evidence = (Class == "1st" & Sex == "Female"))
print(prob_survival_female_1st)
# [1] 0.5120385


# Probabilidad de sobrevivir siendo un menor de 18 años de segunda clase

prob_survival_child_2nd <- cpquery(fitted_bn, 
                                   event = (Survived == "Yes"), 
                                   evidence = (Class == "2nd" & Age == "Child"))
print(prob_survival_child_2nd)
# [1] 0.4956672

# Probabilidad de que un hombre de segunda clase no haya sobrevivido
prob_no_survival_male_2nd <- cpquery(fitted_bn, 
                                     event = (Survived == "No"), 
                                     evidence = (Class == "2nd" & Sex == "Male"))
print(prob_no_survival_male_2nd)
# [1] 0.5081169




# Probabilidad de sobrevivir siendo un pasajero de tercera clase
prob_survival_3rd <- cpquery(fitted_bn, 
                             event = (Survived == "Yes"), 
                             evidence = (Class == "3rd"))
print(prob_survival_3rd)
# [1] 0.5053763

# Probabilidad de sobrevivir siendo un pasajero de primera clase
prob_survival_1st <- cpquery(fitted_bn, 
                             event = (Survived == "Yes"), 
                             evidence = (Class == "1st"))
print(prob_survival_1st)
# [1] 0.4959217




# Crear un dataframe con las probabilidades de supervivencia por clase
prob_class <- data.frame(
  Class = c("1st", "2nd", "3rd"),
  Survival_Prob = c(
    cpquery(fitted_bn, event = (Survived == "Yes"), evidence = (Class == "1st")),
    cpquery(fitted_bn, event = (Survived == "Yes"), evidence = (Class == "2nd")),
    cpquery(fitted_bn, event = (Survived == "Yes"), evidence = (Class == "3rd"))
  )
)

# Graficar las probabilidades de supervivencia por clase
ggplot(prob_class, aes(x = Class, y = Survival_Prob)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Probabilidad de Supervivencia por Clase",
       x = "Clase",
       y = "Probabilidad de Supervivencia") +
  theme_minimal()


# Crear un dataframe con las probabilidades de supervivencia por sexo
prob_sex <- data.frame(
  Sex = c("Male", "Female"),
  Survival_Prob = c(
    cpquery(fitted_bn, event = (Survived == "Yes"), evidence = (Sex == "Male")),
    cpquery(fitted_bn, event = (Survived == "Yes"), evidence = (Sex == "Female"))
  )
)

# Graficar las probabilidades de supervivencia por sexo
ggplot(prob_sex, aes(x = Sex, y = Survival_Prob)) +
  geom_bar(stat = "identity", fill = "lightcoral") +
  labs(title = "Probabilidad de Supervivencia por Sexo",
       x = "Sexo",
       y = "Probabilidad de Supervivencia") +
  theme_minimal()














