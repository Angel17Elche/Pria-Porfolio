library(tidyverse)
library(dplyr)
library(gapminder)

2.1 Usa filter para filtrar por el año 1952 y guarda el resultado en la variable gm_1952.

gm_1952 <- gapminder %>%
  filter(year == 1952)

2.2 Filtra por país (China) y por año (2002) y guarda el resultado en la variable china_2002

china_2002 <- gapminder %>%
  filter(country == 'China', year == 2002)

2.3 Ordena por lifExp en ambos sentidos. Localiza el año más reciente con datos y filtra por éste. ¿Qué países son los extremos?
  
esperanza_vidamin <- gapminder %>%
  filter(lifeExp == min(lifeExp))

esperanza_vidamax <- gapminder %>%
  filter(lifeExp == max(lifeExp))

2.4 Crea una nueva columna popM, con la población en millones de habitantes.

popM <- gapminder %>%
  mutate(popM = pop / 1000000)

2.5 Extrae los 10 países con mayor población, expresados en millones de habitantes en el año 2007. Filtra por esos 10 países y extrae la población de los mismos en 1957.

top10 <- gapminder %>%
  filter(year == 2007) %>%
  arrange(desc(pop)) %>%
  slice(1:10)

top10country2007 <- top10$country

top10in1957 <- gapminder %>%
  filter(country %in% top10country2007, year == 1957) %>%)
print(top10in1957)

2.6 Encuentra el promedio de esperanza de vida (lifeExp) para cada continente y resúmelo en una tabla.

promediocontinente <- gapminder %>%
  group_by(continent) %>%
  summarise(mean_lifeExp = mean(lifeExp))

2.7 Añade una columna llamada gdpPercapLog que sea el logaritmo de gdpPercap, luego organiza los datos por esta nueva columna de forma descendente.

gdp <- gapminder %>%
  mutate(gdpPercapLog = log(gdpPercap)) %>%
  arrange(desc(gdpPercapLog))

2.8 Filtra los países donde la expectativa de vida lifeExp es mayor que 80 años.

viejitos <- gapminder %>%
  filter(lifeExp > 80)

2.9 Calcula la suma del PIB (gdpPercap * pop) para cada país y año, agrupando por país y año.

PIB <- gapminder %>%
  mutate(PIB = gdpPercap * pop) %>%
  group_by(country, year) %>%
  summarise(PIBtotal = sum(PIB))

2.10 Crea una nueva columna que clasifique a los países en ‘Alto PIB per cápita’ si gdpPercap es mayor que 10000, y ‘Bajo PIB per cápita’ en caso contrario.

tipoPIB <- gapminder %>%
  mutate(alto_PIB = gdpPercap > 10000) %>%
  mutate(bajo_PIB = gdpPercap < 10000)
o
gapminder %>%
  mutate(tipo_pib =
           case_when(gdpPercap > 10000 ~ "Alto PIB per cápita",
                     TRUE ~ "Bajo PIB per cápita" )
  )

2.11 Utiliza mutate_if para transformar todas las columnas numéricas aplicando el logaritmo natural, pero sólo si todas las entradas en esa columna son positivas.

columnaspos <- gapminder %>%
  mutate_if(is.numeric, function(x) if(all(x > 0)) log(x) else x)

2.12 Para cada continente, encuentra el país con la mayor población (pop) cada año.

paises_mayor_poblacion <- gapminder %>%
  group_by(continent, year) %>%
  filter(pop == max(pop))
  
2.13 Obten el mínimo, el máximo y la media de la esperanza de vida (lifeExp) y el PIB per cápita (gdpPercap) por continente.

mimaxme <- gapminder %>%
  group_by(continent) %>%
  summarise(
    EspeMin = min(lifeEx, na.rm = TRUE),
    EspeMax = max(lifeExp, na.rm = TRUE),
    EspeMed = mean(lifeExp, na.rm = TRUE),
    PIBMin = min(gdpPercap, na.rm = TRUE),
    PIBMax = max(gdpPercap, na.rm = TRUE),
    PIBMed = mean(gdpPercap, na.rm = TRUE ))





print('GGPLOT2')

library(ggplot2)
library(dplyr)
library(gapminder)

3.1 Usando la variable gm_1952, crea un diagrama de dispersión donde se represente la renta per cápita en función de la población. Ahora la esperanza de vida en función de la población.

gm_1952 %>%
  ggplot(aes(x = pop, y = gdpPercap)) +
  geom_point() 

ggplot(aes(x = pop, y = lifeExp)) +
  geom_point()

3.2 Aplica a las gráficas anteriores un escalado logarítmico (log10) en el eje x e y

gm_1952 %>%
  ggplot(aes(x = pop, y = gdpPercap)) +
  geom_point() + 
  scale_x_log10() +
  scale_y_log10()

gm_1952 %>%
  ggplot(aes(x = pop, y = lifeExp)) +
  geom_point() + 
  scale_x_log10() +
  scale_y_log10()

3.3 Realiza un diagrama de dispersión de la esperanza de vida en función de la población, pero mapea la variable continent por color. Usa escalado para el eje x

gm_1952 %>%
  ggplot(aes(x = pop, y = lifeExp, color = continent)) +
  geom_point() + 
  scale_x_log10() 

3.4 Repite la representación anterior pero añade el mapeo de la renta per cápita por tamaño del punto

gm_1952 %>%
  ggplot(aes(x = pop, y = lifeExp, size = gdpPercap, color = continent)) +
  geom_point() + 
  scale_x_log10() 

3.5 Ahora, en lugar de mapear por continente, utiliza contienente como variable “faceta” y genera 5 gráficas en un lienzo con la esperanza de vida en función de la población en 1952.

gm_1952 %>%
  ggplot(aes(x = pop, y = lifeExp, size = gdpPercap)) +
  geom_point() + 
  scale_x_log10() +
  facet_wrap(~ continent)

3.6 Representa la esperanza de vida en función del año, usando facetas (por continente). Utiliza la función de agregación apropiada. Prueba diagramas de dispersión, líneas ó barras.

gapminder %>%
  mutate(pop = pop/1000000) %>%
  group_by(year, continent) %>%
  summarise(lifeExp = median(lifeExp), pop = median(pop)) %>%
  ggplot(aes(x = year, y = lifeExp, size = pop)) +
  geom_point() +
  facet_wrap(~ continent)

3.7 Representa la población total en función del año, usando facetas (por continente). Utiliza la función de agregación apropiada.

gapminder %>%
  mutate(pop = pop/1000000) %>%
  group_by(year, continent) %>%
  summarise(pop = sum(pop)) %>%
  ggplot(aes(x = year, y = pop)) +
  geom_line() +
  facet_wrap(~ continent, scales = "free")

3.8 Representa en una sola gráfica, sin el uso de facetas, la información de los dos ejercicios anteriores, pero utiliza colores para mapear los continentes.

gapminder %>%
  mutate(pop = pop/1000000) %>%
  group_by(year, continent) %>%
  summarise(lifeExp = median(lifeExp), pop = median(pop)) %>%
  ggplot(aes(x = year, y = lifeExp, size = pop, color = continent)) +
  geom_point() 

3.9 Representa la renta per cápita en 2007 mapeando los puntos del diagrama de dispersión por color según el contienente y por tamaño, según la población.

gapminder %>%
  filter(year == 2007) %>%
  ggplot(aes(x = pop, y = gdpPercap, color = continent, size = pop)) +
  geom_point() + 
  scale_x_log10() +
  scale_y_log10()

3.10 Representa la evolución de la renta per cápita año a año entre Japón vs EEUU en una gráfica de líneas.

gapminder %>%
  filter(country == "Japan" | country == "EEUU") %>%
  ggplot(aes(x = pop, y = gdpPercap, color = continent, size = pop)) +
  geom_point() + 
  scale_x_log10() +
  scale_y_log10()

3.11 Representa un diagrama de barras para comparar la renta per cápita en 2007 de europa vs EEUU+Canada.

gapminder %>%
  filter(year == 2007) %>%
  filter(country %in% 
           c("Canada", "United States") | continent == "Europe" ) %>%
  mutate(continent = if_else(continent == "Europe", "Europa", "EEUU+Canada")) %>%
  mutate(PIB = gdpPercap*pop) %>%
  group_by(continent) %>%
  summarise(PIB = sum(PIB), pop = sum(pop)) %>%
  mutate(gdpPercap = PIB/pop) %>%
  ggplot(aes(x = continent, y = gdpPercap)) +
  geom_col()

3.12 Dibuja un histograma con los datos de la población mundial en 2007 según gapminder.

gapminder %>%
  filter(year == 2007) %>%
  ggplot(aes(pop)) +
  geom_histogram(bins = 39)

3.13 Modifica la gráfica usando escala logarítmica. Prueba realizar diagrama de densidad en lugar de un histograma.

gapminder %>%
  filter(year == 2007) %>%
  ggplot(aes(pop)) +
  geom_histogram(bins = 39) + 
  scale_x_log10()