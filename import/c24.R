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

