#Se varga libreria para carga de datos desde github
library(RCurl)

#Carga de la base de datos
url_casetas<-"https://raw.githubusercontent.com/Ricardo27cruz27/Algoritmo-Kruskal/master/datos/casetas.csv"
x_casetas<-getURL(url_casetas)
url_coordenadas<-"https://raw.githubusercontent.com/Ricardo27cruz27/Algoritmo-Kruskal/master/datos/coordenadas.csv"
x_coordenadas<-getURL(url_coordenadas)
url_km<-"https://raw.githubusercontent.com/Ricardo27cruz27/Algoritmo-Kruskal/master/datos/km.csv"
x_km<-getURL(url_km)

#setwd("~/Maestria/CIMAT/2 Semestre/Optimizacion y numerico/proyecto")

#librerias para los mapas
library(mapdata)
library(ggplot2)

mexico<-map_data("worldHires", "Mexico")
data<-read.csv(text=x_coordenadas,header = T)
km<-read.csv(text=x_km,header = T,row.names = 1)
casetas<-read.csv(text=x_casetas,header = T,row.names = 1)

NAmap <- ggplot() + geom_polygon(data = mexico, 
                                 aes(x=long, y = lat, group = group), 
                                 fill = "white", 
                                 color="black") 
NAmap2 <- ggplot() + geom_polygon(data = mexico, 
                                 aes(x=long, y = lat, group = group), 
                                 fill = "white", 
                                 color="gray") 
#mapa con nodos
mn<-NAmap+geom_point(data = as.data.frame(data),
                 aes(y=Latitud,x=Longitud),
                 shape=21, size=5.0)
#mapa con fondo
#NAmap + theme(line = element_blank(),
 #                      text = element_blank(), 
  #                     panel.background = element_rect(fill = "steelblue"))

#mapa con texto
map_txt<-NAmap+geom_text(data=as.data.frame(data),
            aes(y=Latitud,x=Longitud,label=abreviacion),
            col="red")


#solo texto
txt<-ggplot()+geom_text(data=as.data.frame(data),
          aes(y=Latitud,x=Longitud,label=abreviacion),
          col="red")

#solo nodos
nodos<-ggplot()+geom_point(data = as.data.frame(data),
                 aes(y=Latitud,x=Longitud),
                 shape=21, size=5.0)



#matriz de adyacencia
adj<-matrix(as.numeric(km>0),32,32)
rownames(adj)<-data$abreviacion
colnames(adj)<-data$abreviacion


lines<-nodos
x<-c()
y<-c()
xend<-c()
yend<-c()
for(i in 1:31){
  for (j in (i+1):32) {
    if(adj[i,j]==1){
      x=c(x,data$Longitud[i])
      y=c(y,data$Latitud[i])
      xend=c(xend,data$Longitud[j])
      yend=c(yend,data$Latitud[j])
      #print(lines)
    }
  }
}

red<-NAmap2+geom_segment(aes(x=x,
                      y=y,
                      xend=xend,
                      yend=yend),
                      color="darkblue")+
  geom_point(data = as.data.frame(data),
              aes(y=Latitud,x=Longitud),
              shape=21, size=5.0)



