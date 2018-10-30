############################################################################################################################
########################################             LIBRARY LOAD                ###########################################
############################################################################################################################

#library(readxl)
#library(data.table)
#library(stringr)
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
  cat(paste("c(\"", paste(colnames(tabla.datos), sep=" ", collapse = "\", \"")), "\")", sep="",collapse="")
}

cols.tabla <- function(tabla.datos){
  cat(paste("c(", paste(colnames(tabla.datos), sep="", collapse = ", ")), ")", sep="",collapse="")
}


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
      return(ficheros.total)
    }
    
  }
}