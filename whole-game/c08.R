#####################################
###                               ###
###         Capítulo 8            ###
###      Flujo de trabajo         ### 
###       Obtener ayuda           ###
###                               ###
#####################################

# Crear un reprex (ejemplo mínimamente reproducible)
y <- 1:4
mean(y)
# reprex() : crea un archivo markdown (.md) con el código que se marca
# (y el resultado de su ejecución)para poder mostrarlo en github.
reprex::reprex() # Para que funcione la salida de reprex hay que marcar
# con el mouse el código que se quiere reproducir en el archivo .md
# produce: 
# un archivo que lo renombré c08-reprex.md


