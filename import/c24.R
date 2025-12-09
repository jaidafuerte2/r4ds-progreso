#####################################
###                               ###
###         Capítulo 23           ###
###        Web Scrapping          ###
###                               ###
#####################################

library(tidyverse)
library(rvest)

# Leer página html:
html <- read_html("http://rvest.tidyverse.org/")
# Producir un xml_document
html # produce:
#> {html_document}
#> <html lang="en">
#> [1] <head>\n<meta http-equiv="Content-Type" content="text/html; charset=UT ...
#> [2] <body>\n    <a href="#container" class="visually-hidden-focusable">Ski ...
   
# Escribir un párrafo y una lista html con R   
html <- minimal_html("
  <p>This is a paragraph</p>
  <ul>
    <li>This is a bulleted list</li>
  </ul>
")
html
#> {html_document}
#> <html>
#> [1] <head>\n<meta http-equiv="Content-Type" content="text/html; charset=UT ...
#> [2] <body>\n<p>This is a paragraph</p>\n  <ul>\n<li>This is a bulleted lis ...

# Crear un un encabezado de primer nivel, un párrafo  con un id 'first',
# y otro párrafo con una clase 'important'
html <- minimal_html("
  <h1>This is a heading</h1>
  <p id='first'>This is a paragraph</p>
  <p class='important'>This is an important paragraph</p>
")

# html_elements() puede encontrar los elementos que coinicidan con
# un selector:
html |> html_elements("p") # produce:
#> {xml_nodeset (2)}
#> [1] <p id="first">This is a paragraph</p>
#> [2] <p class="important">This is an important paragraph</p>
html |> html_elements(".important")
#> {xml_nodeset (1)}
#> [1] <p class="important">This is an important paragraph</p>
html |> html_elements("#first")
#> {xml_nodeset (1)}
#> [1] <p id="first">This is a paragraph</p>

# html_element() devuelve la primera coinicidencia de una etiqueta
html |> html_element("p")
#> {html_node}
#> <p id="first">

# html_elements() devuelve un vector de longitud cero cuando se usa un
# selector que no coincide con ningún elemento del documento
html |> html_elements("b") # produce:
#> {xml_nodeset (0)}
# html_element() devuelve un valor faltante cuando se usa un selector
# que no coincide con ningún elemento del documento
html |> html_element("b")
#> {xml_missing}
#> <NA>

# Crear página html básica con una lista desordenada (ul) donde cada 
# elemento (li) contiene información sobre 4 presonajes de star wars
html <- minimal_html("
  <ul>
    <li><b>C-3PO</b> is a <i>droid</i> that weighs <span class='weight'>167 kg</span></li>
    <li><b>R4-P17</b> is a <i>droid</i></li>
    <li><b>R2-D2</b> is a <i>droid</i> that weighs <span class='weight'>96 kg</span></li>
    <li><b>Yoda</b> weighs <span class='weight'>66 kg</span></li>
  </ul>
  ")

# Usar html_elements para crear un vector donde cada elemento corresponde 
# a un personaje diferente:
characters <- html |> html_elements("li")
characters # produce:
#> {xml_nodeset (4)}
#> [1] <li>\n<b>C-3PO</b> is a <i>droid</i> that weighs <span class="weight"> ...
#> [2] <li>\n<b>R4-P17</b> is a <i>droid</i>\n</li>
#> [3] <li>\n<b>R2-D2</b> is a <i>droid</i> that weighs <span class="weight"> ...
#> [4] <li>\n<b>Yoda</b> weighs <span class="weight">66 kg</span>\n</li>

# Para extraer sólo el nombre del personaje se usa html_element() pues
# sólo devuelve una respuesta por elemento, ignora todo lo demás
characters |> html_element("b") # produce:
#> {xml_nodeset (4)}
#> [1] <b>C-3PO</b>
#> [2] <b>R4-P17</b>
#> [3] <b>R2-D2</b>
#> [4] <b>Yoda</b>

# Encontrar todas las etiquetas que tienen la clase .weight (que están 
# dentro de  span)
characters |> html_elements(".weight")
#> {xml_nodeset (3)}
#> [1] <span class="weight">167 kg</span>
#> [2] <span class="weight">96 kg</span>
#> [3] <span class="weight">66 kg</span>

# Extraer el contenido de texto simple de un elemento html con la
# función html_text2()
characters |> 
  html_element("b") |> 
  html_text2() # produce:
#> [1] "C-3PO"  "R4-P17" "R2-D2"  "Yoda"
#
characters |> 
  html_element(".weight") |> 
  html_text2() # produce:
#> [1] "167 kg" NA       "96 kg"  "66 kg"

# Extraer datos de los atributos con html_attr()
html <- minimal_html("
  <p><a href='https://en.wikipedia.org/wiki/Cat'>cats</a></p>
  <p><a href='https://en.wikipedia.org/wiki/Dog'>dogs</a></p>
")

html |> 
  html_elements("p") |> 
  html_element("a") |> 
  html_attr("href")
#> [1] "https://en.wikipedia.org/wiki/Cat" "https://en.wikipedia.org/wiki/Dog"
# NOTA: html_attr() devuelve cadenas, sólo cadenas, no número ni fechas

# Mostrar una tabla html simple con dos columnas y tres filas:
html <- minimal_html("
  <table class='mytable'>
    <tr><th>x</th>   <th>y</th></tr>
    <tr><td>1.5</td> <td>2.7</td></tr>
    <tr><td>4.9</td> <td>1.3</td></tr>
    <tr><td>7.2</td> <td>8.1</td></tr>
  </table>
  ")
# Devolver una tibble basada en la tabla de una página gracias a la
# función html_table()
html |> 
  html_element(".mytable") |> 
  html_table() # produce:
#> # A tibble: 3 × 2
#>       x     y
#>   <dbl> <dbl>
#> 1   1.5   2.7
#> 2   4.9   1.3
#> 3   7.2   8.1

# Leer el html de la página starwars.html
url <- "https://rvest.tidyverse.org/articles/starwars.html"
html <- read_html(url)
# Extraer los elementos <section>
section <- html |> html_elements("section")
section # produce:
#{xml_nodeset (7)}
#[1] <section><h2 data-id="1">\nThe Phantom Menace\n</h2>\n<p>\nRelease ...
#[2] <section><h2 data-id="2">\nAttack of the Clones\n</h2>\n<p>\nRelea ...
#[3] <section><h2 data-id="3">\nRevenge of the Sith\n</h2>\n<p>\nReleas ...
#[4] <section><h2 data-id="4">\nA New Hope\n</h2>\n<p>\nReleased: 1977- ...
#[5] <section><h2 data-id="5">\nThe Empire Strikes Back\n</h2>\n<p>\nRe ...
#[6] <section><h2 data-id="6">\nReturn of the Jedi\n</h2>\n<p>\nRelease ...
#[7] <section><h2 data-id="7">\nThe Force Awakens\n</h2>\n<p>\nReleased ...

# Extraer el texto de los elemetos h2
section |> html_element("h2") # produce:
#{xml_nodeset (7)}
#[1] <h2 data-id="1">\nThe Phantom Menace\n</h2>
#[2] <h2 data-id="2">\nAttack of the Clones\n</h2>
#[3] <h2 data-id="3">\nRevenge of the Sith\n</h2>
#[4] <h2 data-id="4">\nA New Hope\n</h2>
section |> html_element("h2") |> html_text2()
#> [1] "The Phantom Menace"      "Attack of the Clones"   
#> [3] "Revenge of the Sith"     "A New Hope"             
#> [5] "The Empire Strikes Back" "Return of the Jedi"     
#> [7] "The Force Awakens"
#
# Extraer el texto de la clase director
section |> html_element(".director") # produce: 
{xml_nodeset (7)}
#[1] <span class="director">George Lucas</span>
#[2] <span class="director">George Lucas</span>
#[3] <span class="director">George Lucas</span>
#[4] <span class="director">George Lucas</span>
#[5] <span class="director">Irvin Kershner</span>
section |> html_element(".director") |> html_text2()
#> [1] "George Lucas"     "George Lucas"     "George Lucas"    
#> [4] "George Lucas"     "Irvin Kershner"   "Richard Marquand"
#> [7] "J. J. Abrams"

# Así que , sabiendo hacer esto, se pueden agrupar todos los resultados 
# en una tibble:
tibble(
  title = section |> 
    html_element("h2") |> 
    html_text2(),
  # released es un caso especial porque lleva fechas no texto, así
  # que necesita un tratamiento distinto y un poco especial
  released = section |> 
    html_element("p") |> 
    html_text2() |> 
    str_remove("Released: ") |> 
    parse_date(),
  director = section |> 
    html_element(".director") |> 
    html_text2(),
  intro = section |> 
    html_element(".crawl") |> 
    html_text2()
) # produce:
#> # A tibble: 7 × 4
#>   title                   released   director         intro                  
#>   <chr>                   <date>     <chr>            <chr>                  
#> 1 The Phantom Menace      1999-05-19 George Lucas     "Turmoil has engulfed …
#> 2 Attack of the Clones    2002-05-16 George Lucas     "There is unrest in th…
#> 3 Revenge of the Sith     2005-05-19 George Lucas     "War! The Republic is …
#> 4 A New Hope              1977-05-25 George Lucas     "It is a period of civ…
#> 5 The Empire Strikes Back 1980-05-17 Irvin Kershner   "It is a dark time for…
#> 6 Return of the Jedi      1983-05-25 Richard Marquand "Luke Skywalker has re…
#> # ℹ 1 more row

# Leer la base de datos de películas de internet (IMDb)
url <- "https://web.archive.org/web/20220201012049/https://www.imdb.com/chart/top/"
html <- read_html(url)
#
# Mostrar la tabla de esta base de datos
table <- html |> 
  html_element("table") |> 
  html_table()
table # produce:
# A tibble: 250 × 5
#  ``    `Rank & Title`                 `IMDb Rating` `Your Rating` ``   
#  <lgl> <chr>                                  <dbl> <chr>         <lgl>
#1 NA    "1.\n      The Shawshank Rede…           9.2 "12345678910… NA   
#2 NA    "2.\n      The Godfather\n   …           9.1 "12345678910… NA   
#3 NA    "3.\n      The Godfather: Par…           9   "12345678910… NA   
#4 NA    "4.\n      The Dark Knight\n …           9   "12345678910… NA   
#5 NA    "5.\n      12 Angry Men\n    …           8.9 "12345678910… NA   

# Seleccionar las variables "IMDb rating" y "Rank & title", 
# renombrarlas y poner su información en columnas separadas
ratings <- table |>
  # Se  seleccionan solo las variables `Rank & Title` y `IMDb Rating`
  select(
    rank_title_year = `Rank & Title`,
    rating = `IMDb Rating`
  ) |> 
  mutate(
    # Se reemplaza el texto "\n +" por " "
    rank_title_year = str_replace_all(rank_title_year, "\n +", " ")
  ) |> 
  # Se separa rank. title y year de la variable: rank_title_year
  separate_wider_regex(
    rank_title_year,
    patterns = c(
      rank = "\\d+", "\\. ",
      title = ".+", " +\\(",
      year = "\\d+", "\\)"
    )
  )
ratings # produce:
#> # A tibble: 250 × 4
#>   rank  title                    year  rating
#>   <chr> <chr>                    <chr>  <dbl>
#> 1 1     The Shawshank Redemption 1994     9.2
#> 2 2     The Godfather            1972     9.1
#> 3 3     The Godfather: Part II   1974     9  
#> 4 4     The Dark Knight          2008     9  
#> 5 5     12 Angry Men             1957     8.9
#> 6 6     Schindler's List         1993     8.9
#> # ℹ 244 more rows

# Encontrar un atributo "title" dentro de la tabla principal que
# está dentro de un table data (td) con una etiqueta strong <strong>
html |> 
  html_elements("td strong") |> 
  head() |> 
  html_attr("title") # produce:
#> [1] "9.2 based on 2,536,415 user ratings"
#> [2] "9.1 based on 1,745,675 user ratings"
#> [3] "9.0 based on 1,211,032 user ratings"
#> [4] "9.0 based on 2,486,931 user ratings"
#> [5] "8.9 based on 749,563 user ratings"  
#> [6] "8.9 based on 1,295,705 user ratings"

# Se puede combinar esto último con la anterior tabla aplicando otra
# vez separate_wider_regex() para extraer la parte de los datos que 
# nos interesa: 
ratings |>
  # Extraer el texto del atributo "title" que está en un <strong>
  # dentro de un table data (td)
  mutate(
    rating_n = html |> html_elements("td strong") |> html_attr("title")
  ) |> 
  # Separa rating_n en 3 cadenas de texto. A la segunda nombrarla
  # number
  separate_wider_regex(
    rating_n,
    patterns = c(
      "[0-9.]+ based on ",
      number = "[0-9,]+",
      " user ratings"
    )
  ) |> 
  # Mostrar la nueva variable number en la tabla
  mutate(
    number = parse_number(number)
  )
#> # A tibble: 250 × 5
#>   rank  title                    year  rating  number
#>   <chr> <chr>                    <chr>  <dbl>   <dbl>
#> 1 1     The Shawshank Redemption 1994     9.2 2536415
#> 2 2     The Godfather            1972     9.1 1745675
#> 3 3     The Godfather: Part II   1974     9   1211032
#> 4 4     The Dark Knight          2008     9   2486931
#> 5 5     12 Angry Men             1957     8.9  749563
#> 6 6     Schindler's List         1993     8.9 1295705
#> # ℹ 244 more rows

