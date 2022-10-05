#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("rpart")

graficar_campo  <- function( campo, campo_clase, valores_clase, otro_mes )
{
  
  #quito de grafico las colas del 5% de las densidades
  qA  <- quantile(  dataset[ foto_mes==202103 , get(campo) ] , prob= c(0.05, 0.95), na.rm=TRUE )
  qB  <- quantile(  dataset[ foto_mes==otro_mes , get(campo) ] , prob= c(0.05, 0.95), na.rm=TRUE )
  
  xxmin  <- pmin( qA[[1]], qB[[1]] )
  xxmax  <- pmax( qA[[2]], qB[[2]] )
  
  densidad_A  <- density( dataset[ foto_mes==202103 & get(campo_clase) %in% valores_clase, get(campo) ],
                          kernel="gaussian", na.rm=TRUE )
  
  densidad_B  <- density( dataset[ foto_mes==otro_mes & get(campo_clase) %in% valores_clase, get(campo) ],
                          kernel="gaussian", na.rm=TRUE )
  
  plot( densidad_A,
        col="blue",
        xlim= c( xxmin, xxmax ),
        ylim= c( 0, pmax( max(densidad_A$y), max(densidad_B$y) ) ),
        main= paste0( campo, ",   ", campo_clase, " in ",  paste( valores_clase,collapse=",")) 
  )
  
  lines(densidad_B, col="red", lty=2)
  
  legend(  "topright",  
           legend=c("202103", otro_mes),
           col=c("blue", "red"), lty=c(1,2))
  
}

graficar_campo_sin_clases  <- function( campo, otro_mes )
{
  #quito de grafico las colas del 5% de las densidades
  qA  <- quantile(  dataset[ foto_mes==202103 , get(campo) ] , prob= c(0.05, 0.95), na.rm=TRUE )
  qB  <- quantile(  dataset[ foto_mes==otro_mes , get(campo) ] , prob= c(0.05, 0.95), na.rm=TRUE )
  
  xxmin  <- pmin( qA[[1]], qB[[1]] )
  xxmax  <- pmax( qA[[2]], qB[[2]] )
  
  densidad_A  <- density( dataset[ foto_mes==202103, get(campo) ],
                          kernel="gaussian", na.rm=TRUE )
  
  densidad_B  <- density( dataset[ foto_mes==otro_mes, get(campo) ],
                          kernel="gaussian", na.rm=TRUE )
  
  plot( densidad_A,
        col="blue",
        xlim= c( xxmin, xxmax ),
        ylim= c( 0, pmax( max(densidad_A$y), max(densidad_B$y) ) ),
        main= campo
  )
  
  lines(densidad_B, col="red", lty=2)
  
  legend(  "topright",  
           legend=c("202103", otro_mes),
           col=c("blue", "red"), lty=c(1,2))
  
}

#Aqui comienza el programa
setwd("C:/_MCD/Labo1/code")  #Establezco el Working Directory

#cargo el dataset donde voy a entrenar
dataset  <- fread("./datasets/competencia2_2022_limpieza2.csv.gz")

dataset  <- dataset[  foto_mes >= 202001 ]

#creo la clase_binaria SI={ BAJA+1, BAJA+2 }    NO={ CONTINUA }
dataset[ foto_mes==202101, 
         clase_binaria :=  ifelse( clase_ternaria=="CONTINUA", "NO", "SI" ) ]


meses_entrenamiento <- c("202001","202002","202003","202004","202005","202007","202008","202009","202010","202011","202012","202101","202102")

dir.create( "./exp/",  showWarnings = FALSE ) 
dir.create( "./exp/drifting/", showWarnings = FALSE )
setwd("./exp/drifting/")

pdf("densidades_mcuentas_saldo.pdf")

campos_buenos <-  c("mcuentas_saldo","mcuentas_saldo_ipc")

for( campo in  campos_buenos )
{
  cat( campo, "  " )
  
  for(mes in meses_entrenamiento)
  {
    cat( mes, "  " )
    
    graficar_campo( campo, "clase_ternaria", c( "BAJA+1", "BAJA+2", "CONTINUA" ),mes )
    graficar_campo( campo, "clase_ternaria", c( "BAJA+1", "BAJA+2" ) ,mes)
    graficar_campo( campo, "clase_ternaria", c( "BAJA+2" ) ,mes)
    graficar_campo( campo, "clase_ternaria", c( "BAJA+1" ) ,mes)
    graficar_campo( campo, "clase_ternaria", c( "CONTINUA" ) ,mes)
  }
  
  cat( "202104", "  " )
  graficar_campo( campo, "clase_ternaria", c( "BAJA+1" ) ,"202104")
  
  cat( "202105", "  " )
  graficar_campo_sin_clases( campo,"202105" )
}

dev.off()