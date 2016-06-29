library(shiny)
library(ggplot2)
library(RPostgreSQL)
library(sqldf)
library(plyr)

#Global
colours <- c("yellow","gray64","blue","red","green","deeppink2","turquoise3",
             "orange2","orange4","darkorchid","gold","dimgray","deepskyblue4","firebrick","plum")
ajuste <- par(mar=c(1,4,3,1)+0.6)
drv <- dbDriver("PostgreSQL")
con <- dbConnect(SQLite(), "~/swift_provenance.db")
#con <- dbConnect(drv, dbname = "gecko_peerj",
#                 host = "localhost", port = 5432,
#                 user = "postgres", password = 'postgres')

run_ids <- as.vector(dbGetQuery(con, "select script_run_id from script_run")[,1])

ui <- fluidPage(
  titlePanel(tags$strong('Swift Provenance\'s Profiling')),
  hr(),
  
  #Input
  sidebarPanel(width = 3,
        tags$style("body{background-color:white; color:	#337ab7}"),
        tags$style(".well{font-style: normal; border-style:solid;color:	#337ab7}"),
        tags$style(".active{font-style: normal;color:black}"),
        tags$style(".radio{font-style: normal;color:black}"),
        selectInput(inputId = 'scriptId', label = 'Script Id', choices = run_ids),
    helpText("Choose a workflow execution above."),
    radioButtons(inputId = "downloadFormatt", label = "Choose a format: ",choices = list("png","pdf")),
    downloadButton(outputId = "downloadButton",label = "Download ")
  ),
  
  #Output
  mainPanel( width = 9,
    tabsetPanel(
      tabPanel("Plot",
               fluidRow(
                 splitLayout(cellWidths = c("50%", "50%"),
                             plotOutput(outputId = "plotDuration"),
                             plotOutput(outputId = "plotCpu")),
                 splitLayout(cellWidths = c("50%", "50%"),
                             plotOutput(outputId = "plotBytes"),
                             plotOutput(outputId = "plotMemory")))),
      tabPanel("Summary", verbatimTextOutput("summaryPlot")), 
      tabPanel("Table", tableOutput("table"))
    ),
    tabsetPanel(
      tabPanel("Prediction", plotOutput("plotRegression")),
      tabPanel("Summary", verbatimTextOutput("summaryPrediction"))
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
  output$summaryPlot <- renderPrint({
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
    
    barplot(duration, log = "y", las=2, main = "Apps x Duration", beside=TRUE, col=colours,
            ylab = "Avg Duration Time (secs)", cex.names=0.2) 
    legend("topleft", xpd = TRUE, legend = apps, col=colours, lty = 1, cex = 1, bty="n")
    
  })
  
  #Plot CPU
  output$plotCpu <- renderPlot({ 
    apps <- c(datasetInput()[ ,1]) 
    cpu <- (datasetInput()[ ,3])
    
    barplot(cpu, log = "y", las=2, main= "Apps x Cpu usage", beside=TRUE, col=colours,
            ylab = "Avg Cpu usage", cex.names=0.2)
  })
  
  #Plot Bytes
  output$plotBytes <- renderPlot({ 
    apps <- c(datasetInput()[ ,1]) 
    bytes <- (datasetInput()[ ,4])
    
    barplot(bytes, log = "y", las=2, main= "Apps x Written bytes", beside=TRUE, col=colours,
            ylab = "AVG Written Bytes", cex.names=0.2) 
  })
  
  #Plot Memory
  output$plotMemory <- renderPlot({ 
    apps <- c(datasetInput()[ ,1]) 
    memoria <- (datasetInput()[,5])
    
    barplot(memoria,log = "y", las=2, main= "Apps x Memory",beside=TRUE,col=colours,
            ylab = "AVG Used Memory",cex.names=0.5) 
    
  })
  
  #Plot Regression
  output$plotRegression <- renderPlot({
    
    script_name <- dbGetQuery(con, paste("select script_filename from script_run where script_run_id='",
                                         input$scriptId,"'", sep = ""))
    
    #Query
    input = dbGetQuery(con, paste("select s.script_run_id, f.size from staged_in i, file f, app_exec a, script_run s
                                  where f.file_id = i.file_id
                                  and a.app_exec_id = i.app_exec_id
                                  and a.script_run_id = s.script_run_id
                                  and s.script_filename = '",script_name,"'
                                  and i.file_id not in (select o.file_id from staged_out o)", sep = ""))
    
    duration = dbGetQuery(con, paste("select script_run_id, duration from script_run where script_filename='",script_name, "'", sep = ""))
    
    input$size <- as.numeric(input$size)
    sum.size <- aggregate(input$size ~ input$script_run_id, input, FUN = function(x){sum(as.numeric(x))})
    
    #Join
    colnames(sum.size) <- c("script_run_id", "size")
    result <- join(sum.size, duration)
    
    #Plot
    plot(result$size, result$duration, xlab="Input size", ylab="Total execution time (s)", pch=20, 
         xlim=range(result$size,result$size))
    
    #Linear
    model <- lm(result$duration ~ result$size) # (y~x)
    abline(model, col = colours)
    #Quadratic
    model.2 <- lm(result$duration ~ poly(result$size, 2, raw=TRUE), result)
    curve(coefficients(model.2)[1] + coefficients(model.2)[2]*x + coefficients(model.2)[3]*x*x, add = T, col = colours[2])
    #Description
    model.legend = c("Linear", "Quadratic")
    legend("topleft", xpd = TRUE, model.legend, col=colours, lty = 1, cex = 1, bty="n")
})
  
  #Download plot
  output$down <- downloadHandler(
    filename = function(){
      paste(input$scriptId,input$var3,sep=".")
    },
  content = function(file){
    if(input$var3 == "png")
      png(file)
      else
      pdf(file)
    
    {apps <- c(datasetInput()[ ,1]) 
      bytes <- (datasetInput()[ ,4])
      duration <- (datasetInput()[ ,2])
      cpu <- (datasetInput()[ ,3])
      memoria <- (datasetInput()[,5])
      
      #Duration
      barplot(duration, log = "y", las=2, main = "Tarefa x Duração", beside=TRUE, col=colours,
              ylab = "Tempo médio de duração", xlab = "Tarefa", cex.names=0.2) 
      legend("topleft", xpd = TRUE, legend = apps, col=colours, lty = 1, cex = 1, bty="n")
      
      #CPU
      barplot(cpu, log = "y", las=2, main= "Tarefa x Cpu", beside=TRUE, col=colours,
              ylab = "Avg CPU",xlab = "Tarefa", cex.names=0.2) 
      #Bytes
      barplot(bytes, log = "y", las=2, main= "Tarefa x Bytes escritos", beside=TRUE, col=colours,
              ylab = "Bytes escritos",xlab = "Tarefa", cex.names=0.2) 
      #Memory
      barplot(memoria,log = "y", las=2, main= "Tarefa x Memoria",beside=TRUE,col=colours,
              ylab = "Tempo de duracao",xlab = "Tarefa",cex.names=0.5)
      }
    
    dev.off()
    }
  )
    
}
shinyApp(ui = ui, server = server)
