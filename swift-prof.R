library(shiny)
library(ggplot2)
library('RPostgreSQL')
library("sqldf")

#Global
colours <- c("yellow","gray64","blue","red","green","deeppink2","turquoise3","orange2","orange4","darkorchid","gold","dimgray","deepskyblue4","firebrick","plum")
ajuste <- par(mar=c(1,4,3,1)+0.6)
drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, dbname = "gecko",
                 host = "localhost", port = 5432,
                 user = "postgres", password = 'postgres')

run_ids <- as.vector(dbGetQuery(con, "select script_run_id from script_run")[,1])

ui <- fluidPage(
  titlePanel(tags$strong('Swift provenance profiling')),
  hr(),
  
  #Input
  sidebarPanel(
    selectInput(inputId = 'scriptId', label = 'Script Id', choices = run_ids),
    helpText("Clique na barra acima para selecionar o script desejado")
  ),
  
  #Output
  mainPanel(
    tabsetPanel(
      tabPanel("Plot",
               fluidRow(
                splitLayout(cellWidths = c("50%", "50%"),
                  plotOutput(outputId = "plotDuration"),
                  plotOutput(outputId = "plotCpu")),
                    splitLayout(cellWidths = c("50%", "50%"),
                      plotOutput(outputId = "plotBytes"),
                      plotOutput(outputId = "plotMemory")))),
      tabPanel("Summary", verbatimTextOutput("summary")), 
      tabPanel("Table", tableOutput("table"))
    )
    
  ),
  hr()
)

server <- function(input, output) {
  #Dados
  datasetInput <- reactive({ 
    dbGetQuery(con, paste("select a.app_name, avg(a.duration) as avg_duration,
                          avg(r.percent_cpu) as avg_percent_cpu,
                          avg(r.fs_writes) as avg_fs_writes,
                          avg(r.max_rss) as avg_max_rss
                          from app_exec a, script_run s, resource_usage r
                          where a.script_run_id = s.script_run_id
                          and r.app_exec_id = a.app_exec_id
                          and s.script_run_id = '",input$scriptId,"' 
                          group by a.app_name",sep="")) 
    
  })
  
  #Summary
  output$summary <- renderPrint({
    summary(datasetInput())
  })
  
  #Table
  output$table <- renderTable({ 
    print(data.frame(datasetInput())) 
  })
  
  #Plot Duraçao
  output$plotDuration <- renderPlot({ 
    apps <- c(datasetInput()[ ,1]) 
    duration <- (datasetInput()[ ,2])
    
    barplot(duration, log = "y", las=2, main = "Tarefa x Duração", beside=TRUE, col=colours,
            ylab = "Tempo médio de duração", xlab = "Tarefa", cex.names=0.2) 
    legend("topleft", xpd = TRUE, legend = apps, col=colours, lty = 1, cex = 1, bty="n")
    
  })
  
  #Plot CPU
  output$plotCpu <- renderPlot({ 
    apps <- c(datasetInput()[ ,1]) 
    cpu <- (datasetInput()[ ,3])
    
    barplot(cpu, log = "y", las=2, main= "Tarefa x Cpu", beside=TRUE, col=colours,
            ylab = "Avg CPU",xlab = "Tarefa", cex.names=0.2) 
    legend("topleft", xpd = TRUE, legend = apps, col=colours, lty = 1, cex = 1, bty="n")
    
  })
  
  #Plot Bytes
  output$plotBytes <- renderPlot({ 
    apps <- c(datasetInput()[ ,1]) 
    bytes <- (datasetInput()[ ,4])
    
    barplot(bytes, log = "y", las=2, main= "Tarefa x Bytes escritos", beside=TRUE, col=colours,
            ylab = "Bytes escritos",xlab = "Tarefa", cex.names=0.2) 
    legend("topleft", xpd = TRUE, legend = apps, col=colours, lty = 1, cex = 1, bty="n")
    
  })
  
  #Plot Memory
  output$plotMemory <- renderPlot({ 
    apps <- c(datasetInput()[ ,1]) 
    memoria <- (datasetInput()[,5])
    
    barplot(memoria,log = "y", las=2, main= "Tarefa x Memoria",beside=TRUE,col=colours,
            ylab = "Tempo de duracao",xlab = "Tarefa",cex.names=0.5) 
    legend("topleft",xpd = TRUE, legend = apps,col=colours,lty = 1,cex = 1,bty="n")
    
  })
  
}
shinyApp(ui = ui, server = server)
