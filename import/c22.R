#####################################
###                               ###
###         Capítulo 22           ###
###        Arrow (Flecha)         ###
###                               ###
#####################################

# Hay una alternativa a los archivos CSV que son los archivos en formato
# parquet (usados en big data) y se pueden manipular con la caja de 
# herramientas de Apache Arrow
library(tidyverse)
library(arrow)
#
library(dbplyr, warn.conflicts = FALSE)
library(duckdb)
#> Loading required package: DBI

# NOTA: No se ejecutará el código porque la base de datos que se
# debe cargar es de 9GB , demasiado para este sistema posit cloud free.

#dir.create("data", showWarnings = FALSE)
#
#curl::multi_download(
#  "https://r4ds.s3.us-west-2.amazonaws.com/seattle-library-checkouts.csv",
#  "data/seattle-library-checkouts.csv",
#  resume = TRUE
#)
#> # A tibble: 1 × 10
#>   success status_code resumefrom url                    destfile        error
#>   <lgl>         <int>      <dbl> <chr>                  <chr>           <chr>
#> 1 TRUE            200          0 https://r4ds.s3.us-we… data/seattle-l… <NA> 
#> # ℹ 4 more variables: type <chr>, modified <dttm>, time <dbl>,
#> #   headers <list>

# Leer una base de datos necesita el doble de memoria de su tamaño,
# una base de datos de 9 gb necesita cerca de 18gb de memoria. Así
# que no se usará read_csv() sino arrow::open_dataset():
seattle_csv <- open_dataset(
  sources = "data/seattle-library-checkouts.csv", 
  col_types = schema(ISBN = string()),
  format = "csv"
)

# Entonces, qué hace open_dataset() ? Practicamente sólo muestra  el
# nombre de sus coljmnas con sus tipos de datos
seattle_csv
#> FileSystemDataset with 1 csv file
#> UsageClass: string
#> CheckoutType: string
#> MaterialType: string
#> CheckoutYear: int64
#> CheckoutMonth: int64
#> Checkouts: int64
#> Title: string
#> ISBN: string
#> Creator: string
#> Subjects: string
#> Publisher: string
#> PublicationYear: string
#
# NOTA: Lo único que se almacena en el disco es la primera fila de los
# nombres de las variables o las columnas. Lo demás sólo se cargará en 
# memoria cuando sea necesario.

# Esposible ver lo que realmente hay en la tabla con la función 
# glimpse()
seattle_csv |> glimpse()
#> FileSystemDataset with 1 csv file
#> 41,389,465 rows x 12 columns
#> $ UsageClass      <string> "Physical", "Physical", "Digital", "Physical", "Ph…
#> $ CheckoutType    <string> "Horizon", "Horizon", "OverDrive", "Horizon", "Hor…
#> $ MaterialType    <string> "BOOK", "BOOK", "EBOOK", "BOOK", "SOUNDDISC", "BOO…
#> $ CheckoutYear     <int64> 2016, 2016, 2016, 2016, 2016, 2016, 2016, 2016, 20…
#> $ CheckoutMonth    <int64> 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6,…
#> $ Checkouts        <int64> 1, 1, 1, 1, 1, 1, 1, 1, 4, 1, 1, 2, 3, 2, 1, 3, 2,…
#> $ Title           <string> "Super rich : a guide to having it all / Russell S…
#> $ ISBN            <string> "", "", "", "", "", "", "", "", "", "", "", "", ""…
#> $ Creator         <string> "Simmons, Russell", "Barclay, James, 1965-", "Tim …
#> $ Subjects        <string> "Self realization, Conduct of life, Attitude Psych…
#> $ Publisher       <string> "Gotham Books,", "Pyr,", "Random House, Inc.", "Di…
#> $ PublicationYear <string> "c2011.", "2010.", "2015", "2005.", "c2004.", "c20…

# collect() de la biblioteca arrow fuerza al sistema para que realice
# los cálculos y devuelva los datos. Este código indica el número
# total de préstamos al año.
seattle_csv |> 
  group_by(CheckoutYear) |> 
  summarise(Checkouts = sum(Checkouts)) |> 
  arrange(CheckoutYear) |> 
  collect()
#> # A tibble: 18 × 2
#>   CheckoutYear Checkouts
#>          <int>     <int>
#> 1         2005   3798685
#> 2         2006   6599318
#> 3         2007   7126627
#> 4         2008   8438486
#> 5         2009   9135167
#> 6         2010   8608966
#> # ℹ 12 more rows

# NOTA: Esta forma de trabajar con datos tan grandes no es tan adecuada
# por lo que se puede preferir un mejor formato, el formato parquet. 
# NOTA: También es útil dividir un archivo demasiado grande en archivos 
# un poco más pequeños. En general hay que tratar de evitar dividir o
# particionar archivos de menos de 20mb o mayores a 2gb o particiones
# que generen más de 10mil archivos. También se debería particionar
# filtrando variables (sólo las más importantes)

# Para particionar una tabla primero se debe agrupar porque estos
# grupos definirán como se parte la tabla con group_by() 
# Luego se guardan las particiones en un directorio con un path
# que proveemos:
pq_path <- "data/seattle-library-checkouts"
#
seattle_csv |>
  group_by(CheckoutYear) |>
  write_dataset(path = pq_path, format = "parquet")
#
# Mostrar las particiones que se acaban de producir
tibble(
  files = list.files(pq_path, recursive = TRUE),
  size_MB = file.size(file.path(pq_path, files)) / 1024^2
)
#> # A tibble: 18 × 2
#>   files                            size_MB
#>   <chr>                              <dbl>
#> 1 CheckoutYear=2005/part-0.parquet    109.
#> 2 CheckoutYear=2006/part-0.parquet    164.
#> 3 CheckoutYear=2007/part-0.parquet    178.
#> 4 CheckoutYear=2008/part-0.parquet    195.
#> 5 CheckoutYear=2009/part-0.parquet    214.
#> 6 CheckoutYear=2010/part-0.parquet    222.
#> # ℹ 12 more rows
# NOTA: El archivo de 9gb se reescribió en 18 archivos de parquet

# Leer con open_dataset() los archivos parquet que se crearon, pero
# asignándole un directorio:
seattle_pq <- open_dataset(pq_path)
# 
# NOTA: Esto creó un data set seattle_pq con el que se puede usar 
# dplyr. Por ejemplo se puede contar el número de libros prestados 
# cada mes durante los últimos 5 años
query <- seattle_pq |> 
  filter(CheckoutYear >= 2018, MaterialType == "BOOK") |>
  group_by(CheckoutYear, CheckoutMonth) |>
  summarize(TotalCheckouts = sum(Checkouts)) |>
  arrange(CheckoutYear, CheckoutMonth)

# Consultar la tabla query que se generó a partir de eattle_pq
query
#> FileSystemDataset (query)
#> CheckoutYear: int32
#> CheckoutMonth: int64
#> TotalCheckouts: int64
#> 
#> * Grouped by CheckoutYear
#> * Sorted by CheckoutYear [asc], CheckoutMonth [asc]
#> See $.data for the source Arrow object
#> 
# NOTA: query genera solo la primera fila, para obtener los resultados 
# se debe llamar a collect()
query |> collect()
#> # A tibble: 58 × 3
#> # Groups:   CheckoutYear [5]
#>   CheckoutYear CheckoutMonth TotalCheckouts
#>          <int>         <int>          <int>
#> 1         2018             1         355101
#> 2         2018             2         309813
#> 3         2018             3         344487
#> 4         2018             4         330988
#> 5         2018             5         318049
#> 6         2018             6         341825
#> # ℹ 52 more rows

# No todas las funciones de R están disponibles para arrow. 
# Para saber que funciones de dplyr() están disponibles para arrow
# se debe escribir lo siguiente:
?acero

# Los archivos parquet se gestionan mucho más rápido que los archivos
# .CSV
# 
# Aquí se observa cuanto tarda R en calcular el número de libros 
# prestados cada mes en 2021, cuando los datos se almacenan en un 
# único archivo .csv:
seattle_csv |> 
  filter(CheckoutYear == 2021, MaterialType == "BOOK") |>
  group_by(CheckoutMonth) |>
  summarize(TotalCheckouts = sum(Checkouts)) |>
  arrange(desc(CheckoutMonth)) |>
  collect() |> 
  system.time()
#>    user  system elapsed 
#>  11.951   1.297  11.387
#
# Aquí se observa cuanto tarda R en calcular el número de libros 
# prestados cada mes en 2021, cuando los datos se almacenan en 
# varios archivos parquet
seattle_pq |> 
  filter(CheckoutYear == 2021, MaterialType == "BOOK") |>
  group_by(CheckoutMonth) |>
  summarize(TotalCheckouts = sum(Checkouts)) |>
  arrange(desc(CheckoutMonth)) |>
  collect() |> 
  system.time()
#>    user  system elapsed 
#>   0.263   0.058   0.063
# NOTA: La aceleración es de 100 veces en el rendimiento.

# Otra ventaja de parquet y arrow es que se pueden transformar en 
# una base de datos DuckDB gracias a arrow::to_duckdb()
seattle_pq |> 
  to_duckdb() |>
  filter(CheckoutYear >= 2018, MaterialType == "BOOK") |>
  group_by(CheckoutYear) |>
  summarize(TotalCheckouts = sum(Checkouts)) |>
  arrange(desc(CheckoutYear)) |>
  collect()
#> Warning: Missing values are always removed in SQL aggregation functions.
#> Use `na.rm = TRUE` to silence this warning
#> This warning is displayed once every 8 hours.
#> # A tibble: 5 × 2
#>   CheckoutYear TotalCheckouts
#>          <int>          <dbl>
#> 1         2022        2431502
#> 2         2021        2266438
#> 3         2020        1241999
#> 4         2019        3931688
#> 5         2018        3987569


########################
###
### 22.5.3 Ejercicios
###
########################

# NO REALIZADOS



