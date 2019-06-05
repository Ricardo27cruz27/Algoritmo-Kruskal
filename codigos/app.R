### APLICATIVO PARA LA RUTA OPTIMA 
### PASANDO POR TODAS LAS CAPITALES
### DE LOS ESTADOS DE LA REPUBLICA

library(shiny)
library(RCurl)
library(optrees)

url<-"https://raw.githubusercontent.com/Ricardo27cruz27/Algoritmo-Kruskal/c8e86575ff83ee70bf739a5735dcb2bcd722df88/codigos/mapas.R"
source(url)


# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Ruta óptima de la República"),
   div(style="display:inline-block",
   numericInput("num", label = "Km por litro",
                value = 10,max=19,min=5)),
   div(style="display:inline-block",
       numericInput("precio", label = "$ litro de gasolina",
                value = 20,max=25,min=18)),
   actionButton("action", label = "Calcular"),
   hr(),
   fluidRow(column(2, verbatimTextOutput("value")),
  
            
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$distPlot <- renderPlot({
      if(input$action==0){
        map_txt
      }
     else{
       rendimiento<-input$num
       precio<-input$precio
       gas<-precio*km/rendimiento
       total<-gas+casetas
       pesos<-c()
       for (i in 1:31) {
         for (j in (i+1):32) {
           if(total[i,j]>1){
             pesos<-rbind(pesos,c(i,j,total[i,j]))
           }
         }
       }
       kruskal<-msTreeKruskal(1:32,pesos)
       #lines<-nodos
       x<-c()
       y<-c()
       xend<-c()
       yend<-c()
       costo<-0
       for(i in 1:nrow(kruskal$tree.arcs)){
          x=c(x,data$Longitud[kruskal$tree.arcs[i,1]])
          y=c(y,data$Latitud[kruskal$tree.arcs[i,1]])
          xend=c(xend,data$Longitud[kruskal$tree.arcs[i,2]])
          yend=c(yend,data$Latitud[kruskal$tree.arcs[i,2]])
          min<-min(kruskal$tree.arcs[i,1],
                   kruskal$tree.arcs[i,2])
          max<-max(kruskal$tree.arcs[i,1],
                   kruskal$tree.arcs[i,2])
          costo<-costo+total[min,max]
       }
       titulo<-paste0("COSTO: ",as.character(round(costo,2)))
       arbol<-NAmap2+geom_segment(aes(x=x,
                                      y=y,
                                      xend=xend,
                                      yend=yend),
                                  color="darkblue")+
         geom_point(data = as.data.frame(data),
                    aes(y=Latitud,x=Longitud),
                    shape=21, size=5.0)+
         ggtitle(titulo)
       arbol
       #print(c("El costo es: ",costo))
     }
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

