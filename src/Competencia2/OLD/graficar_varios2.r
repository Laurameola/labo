#Necesita para correr en Google Cloud
#  32 GB de memoria RAM
# 256 GB de espacio en el disco local
#   8 vCPU

#Este script genera graficos que muestra que para algunos meses, ciertas variables
#  fueron pisadas con CEROS por el sector deIT que genera el DataWarehouse

#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection


require("data.table")

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#Aqui comienza el programa

#copio si hace falta el dataset

setwd("C:/_MCD/Labo1/code")  #Establezco el Working Directory

#cargo el dataset
dataset  <- fread("./datasets/competencia2_2022_renov.csv.gz")   #donde entreno


#creo la carpeta donde va el experimento
# DA  representa  Exploratory Data Analysis
dir.create( "./exp/EDA-comp2/", showWarnings = FALSE )
setwd("./exp/EDA-comp2/")   #Establezco el Working Directory DEL EXPERIMENTO



#ordeno el dataset
setorder( dataset,  foto_mes )

campos_buenos  <- setdiff( colnames( dataset), 
                           c("numero_de_cliente","foto_mes","clase_ternaria" ) )


#------------------------------------------------------------------------------
#Para cada variable , grafico para cada mes el ratio de ceros que tiene esa variable

pdf("zeroes_ratio_renov.pdf")

for( campo in  campos_buenos )
{
  tbl <- dataset[ foto_mes<=202105,
                  list( "zero_ratio" = sum(get(campo)==0, na.rm=TRUE)/.N ) ,
                  foto_mes ]

  ymin <-  min( tbl$zero_ratio )
  ymax <-  max( tbl$zero_ratio )
  if( ymin == 0 )  ymin <- -0.1
  if( ymax == 0 )  ymax <-  0.1

  plot(x= 1:nrow(tbl),
       y= tbl$zero_ratio,
       type= "o",
       main= paste0("Zeroes ratio  -  ",  campo),
       xlab= "Periodo",
       ylab= "Zeroes  ratio",
       ylim= c( ymin, ymax ),
       xaxt= "n"
     )

  axis(1, at=1:nrow(tbl), labels=tbl$foto_mes)

  abline(v=c(1,13,25), col=c("green","green","green"), lty=c(1,1,1), lwd=c(1,1,1))
  abline(v=c(7,19,31), col=c("green","green","green"), lty=c(3,3,3), lwd=c(1,1,1))
}

dev.off()

#------------------------------------------------------------------------------
#Para cada variable , grafico para cada mes el ratio de NAs que tiene esa variable


#------------------------------------------------------------------------------

#dejo la marca final
cat( format(Sys.time(), "%Y%m%d %H%M%S"),"\n",
     file= "zRend.txt",
     append= TRUE  )

