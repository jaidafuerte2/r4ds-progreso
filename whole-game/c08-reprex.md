``` r
# Crear un reprex (ejemplo mínimamente reproducible)
y <- 1:4
mean(y)
#> [1] 2.5
# reprex() : crea un archivo markdown (.md) con el código que se marca
# (y el resultado de su ejecución)para poder mostrarlo en github.
reprex::reprex()
#> ℹ Non-interactive session, setting `html_preview = FALSE`.
#> CLIPR_ALLOW has not been set, so clipr will not run interactively
#> Error in switch(where, expr = stringify_expression(x_expr), clipboard = ingest_clipboard(), : EXPR must be a length 1 vector
```

<sup>Created on 2025-10-08 with [reprex v2.1.1](https://reprex.tidyverse.org)</sup>
