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


