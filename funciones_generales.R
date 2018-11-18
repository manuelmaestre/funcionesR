############################################################################################################################
########################################             LIBRARY LOAD                ###########################################
############################################################################################################################

#library(readxl)
library(data.table)
library(stringr)
#library(rgdal)

############################################################################################################################
########################################             FUNCTION DEFS               ###########################################
############################################################################################################################

# Copy data out of R
copy.table <- function(obj, size = 4096) {
  clip <- paste('clipboard-', size, sep = '')
  f <- file(description = clip, open = 'w')
  write.table(obj, f, row.names = FALSE, sep = '^', na="")
  close(f)  
}

# Paste data into R

paste.table <- function() {
  f <- file(description = 'clipboard', open = 'r')
  df <- read.table(f, sep = '\t', header = TRUE)
  close(f)
  return(df)
}

cols.tabla.comillas <- function(tabla.datos){
  #return(str_replace(as.character(cat(paste("c(\"", paste(colnames(tabla.datos), sep=" ", collapse = "\", \"")), "\")", sep="",collapse="")), " ", ""))
  return(as.character(cat(paste("c(\"", paste(colnames(tabla.datos), sep=" ", collapse = "\", \"")), "\")", sep="",collapse="")))
}
  
cols.tabla <- function(tabla.datos){
  return(str_replace(as.character(cat(paste("c(", paste(colnames(tabla.datos), sep="", collapse = ", ")), ")", sep="",collapse="")), " ", ""))
}

#### Devuelve un data.table con los ficheros existentes en el directorio de entrada que tienen la extensión indicada en el parametro de entrada
#### realmente que terminan como lo indicado en el parametro extension. Agrega los datos básicos de los ficheros: ruta, nombre, tamaño, fecha de modifcicacion
#### in.process.dir <- directorio que contiene los ficheros a listar, en formato de autocompletado de R
#### extension.fichero, ultimos caracteres que debe tener el fichero para que sea incluido en el resultado

lista.ficheros <- function(in.process.dir, extension.fichero){
  
  if (dir.exists(in.process.dir)){
    ficheros <-  list.files(in.process.dir, full.names = T)
    if (length(ficheros)>0){
      for (in.process.file in ficheros){
        nombre.fichero <- strsplit(in.process.file, "/")[[1]]
        nombre.fichero <- nombre.fichero[length(nombre.fichero)]
        
        ## Cargamos los ficheros eliminando las filas sin datos que pueda haber
        
        
        if (exists("ficheros.total")){
          ficheros.total <- rbind(ficheros.total, data.table( RutaCompleta = in.process.file,
                                                              NombreFichero = nombre.fichero,
                                                              FechaModificacion = file.mtime(in.process.file),
                                                              Size = file.size(in.process.file)))
        }
        
        
        if (!exists("ficheros.total")){
          ficheros.total <- data.table( RutaCompleta = in.process.file,
                                        NombreFichero = nombre.fichero,
                                        FechaModificacion = file.mtime(in.process.file),
                                        Size = file.size(in.process.file))
        }
      }
      
      
      ## Eliminamos los ficheros que no tengan la extension
      
      ficheros.total[, str_to_upper(str_sub(NombreFichero, start = str_length(NombreFichero)-(str_length(extension.fichero)-1))) == toupper(extension.fichero)]
      
      ficheros.total <- ficheros.total[str_to_upper(str_sub(NombreFichero, start = str_length(NombreFichero)-(str_length(extension.fichero)-1))) == toupper(extension.fichero),]
      if (nrow(ficheros.total)==0){ficheros.total <- 0}
      return(ficheros.total)
    }
    
  }
}


#### Formatea un frame o data.table de entrada a unas columnas de interes. Si la columna no existe en el frame de entrada, se crea y se le asignan NA
#### tabla.formatear <- frame de entrada que se quiere formatear a unos nombres y posiciones de columna indicados por colnombres.salida
#### colnombres.salida <- nombre y posición de las columnas que debe tener el frame de salida

formatea.tabla.lista <- function(tabla.formatear, colnombres.salida){
  
  ##verificacion formatos de entrada
  
  if (is.data.frame(tabla.formatear) == F | is.vector(colnombres.salida) == F){
    resultado <- 0} else{
      
      tabla.formatear <- as.data.table(tabla.formatear)
      columnas.insertar <- colnombres.salida[!colnombres.salida %in% colnames(tabla.formatear)]
      tabla.formatear[, (columnas.insertar) := NA]
      setcolorder(tabla.formatear, colnombres.salida)
      resultado <- tabla.formatear <- tabla.formatear[, 1:length(colnombres.salida)]
      
    }
  
  return (resultado)
}


#### Integra ficheros de texto contenidos en un directorio en un único fichero, añadiendo a este los datos de los ficheros de entrada. Tamaño, nombre y fecha
#### parametros:
#### directorio.contenedor: directorio que contiene los ficheros a integrar (con formato autocompletado R)
#### terminacion.ficheros: caracteres por la izquierda que deben contener los nombres de los ficheros a procesar
#### separador: caracter a considerar como separador de columans
#### salida: un data.table con el contenido de los ficheros integrados, más las columnas identificando el fichero origen

integra.ficheros.directorio <- function(directorio.contenedor, terminacion.ficheros, separador){
  
  ficheros.procesar <- lista.ficheros(directorio.contenedor, 'csv')
  
  
  if (nrow(ficheros.procesar)>0){
    
    for (i in seq(nrow(ficheros.procesar))){
      
      
      fila <- ficheros.procesar[i]
      print(nrow(ficheros.procesar)-i)
      print(fila$NombreFichero)
      if (.Platform$OS.type == "windows") flush.console()
      
      ## Intentamos abrir el fichero
      
      error.open.file <- try(data.table(read.csv(as.character(fila$RutaCompleta), header = T, sep = separador, colClasses = 'character', strip.white = T, encoding = 'UTF-8')))
      
      if (class(error.open.file) != "try-error"){
        
        nuevos.datos <- data.table(read.csv(as.character(fila$RutaCompleta), header = T, sep = separador, colClasses = 'character', strip.white = T, encoding = 'UTF-8'))
        nuevos.datos$fichero.origen.integrado <- fila$NombreFichero
        nuevos.datos$fecha.fichero.integrado <- fila$FechaModificacion
        nuevos.datos$size.fichero.integrado <- fila$Size
        
        if (i == 1){
          total.datos <- nuevos.datos
        } else {
          nuevos.datos <- formatea.tabla.lista(nuevos.datos, colnames(total.datos))
          if (!is.data.frame(nuevos.datos)){} else{
            total.datos <- rbind(total.datos, nuevos.datos)
          }
        }
      }
      
      if (class(error.open.file) == "try-error"){
      }
    }
    if (nrow(total.datos)==0){resultado <- 0}else{resultado <- total.datos}
    return(resultado)
  }
}


