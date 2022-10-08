
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
dataset  <- fread("./datasets/competencia2_2022.csv.gz")   #donde entreno

nrow(dataset)
dataset <- dataset[foto_mes!=202006]
nrow(dataset)

fwrite( dataset,
        file="./datasets/competencia2_2022_renov.csv.gz",
        sep= "," )