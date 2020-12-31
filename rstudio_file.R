instal.packages(dplyr)
instal.packages(dplyr)
instal.packages(zoo)
instal.packages(recosystem)

library(dplyr)
library(dplyr)
library(zoo)
library(recosystem)
####Desempaquetamos el .tar
#untar("C:/Users/Tomas/Desktop/BI/proyecto/training_set.tar")

####cargamos los archivos
filelist = list.files(pattern = "*.txt")

####Cargar los archivos en una variable de archivos con sus movie_id.
datalist2 = lapply(filelist, 
function(x){
  df <- read.table(x,sep=",",skip=1, header=F);
  colnames(df) <- c("user_id","rating","date")
  df[,"movie_id"] <- gsub(":", "", readLines(x, n=1));
  return(df);
}
)
####Unimos las listas en 1 dataframe:
boundrows <- bind_rows(datalist2)
#simplificamos un poco los datos para comenzar a evaluar los datos:
#seleccionamos solo las columnas de user_id, rating, movie_id
br <- select(boundrows,user_id,rating,movie_id)
mw <- sort(table(br$movie_id), decreasing=TRUE)[1:1777]
dfmw <- enframe(mw)
br_mw <- filter(br, movie_id %in% dfmw$name) #76mill de datos
rm(br)
br<-br_mw
#se genera una archivo y un dataframe de ejemplo con movie_id y user_id para precedir ranking
predecir <- read.table("predecir.txt",sep=",",header=FALSE,fill=TRUE)
#se genera un modelo de dataframe parecido al modelo br para ser trabajados
predecir $V2 <- predecir $V1
predecir $V1[!grepl(":",predecir $V1)] <- NA
predecir$V1 <- gsub(":","",predecir$V1)
predecir$V1 <- na.locf(predecir$V1)
predecir <- filter(predecir,!grepl(":",predecir$V2))
#se crean los nombres a las filas correspondientes
names(predecir) <- c("movie_id","user_id")
#se crea un parametro rankin que a futuro podria ser la calificacion real del usuario
predecir$rating <- NA
#se comienza con la libreria recosystem para la recomendacion de peliculas
set.seed(123)
r=Reco()
#se comienza a modelar el entrenamiento añadiendo los datos necesarios
opciones <- r$tune(data_memory(br$user_id,br$movie_id, rating=br$rating, index1=TRUE),
opts=list(dim=c(5), lrate=c(0.05),  niter=1, nfold=5, verbose=FALSE))
#comienza el entrenamiento
r$train(data_memory(br$user_id, br$movie_id, rating=br$rating, index1=TRUE),
opts=c(opciones$min, nthread=1, niter=50))
predecirranking <- r$predict(data_memory(predecir$user_id,predecir$movie_id,
 rating=NULL, index1=TRUE),out_memory())
#los valores resultantes son almacenados dentro de la variable predecida por el modelo
predecir$predecir_rating <- predecirranking
df1<-filter(predecir,user_id==859819)
df2<-filter(df1,predecir_rating>4.5)