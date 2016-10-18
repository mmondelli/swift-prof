function(input, output, clientData, session) {
  
  #Data = NULL to avoid errors
  v <- reactiveValues(data = NULL)
  cpu <- reactiveValues(data = NULL)
  
  observe({
    ids <- reactive({
      dbGetQuery(con, paste("select script_run_id from script_run where script_filename = '",
                            input$scriptName,"' and start_time between '", as.Date(input$dateId[1], "%Y-%m-%d"), "%' and '", 
                            as.Date(input$dateId[2], "%Y-%m-%d"), "%'", sep=""))
    }) #fim reactive
    
    updateSelectInput(session, inputId = "scriptId", choices = ids())
  })#fim observe
  
  #Data
  observeEvent(input$updateButton, {
    v$data <- dbGetQuery(con, paste("select a.app_name, avg(a.duration) as avg_duration,
                                    avg(r.percent_cpu) as avg_percent_cpu,
                                    avg(r.fs_writes) as avg_fs_writes,
                                    avg(r.fs_reads) as avg_fs_reads,
                                    avg(r.max_rss) as avg_max_rss
                                    from app_exec a, script_run s, resource_usage r
                                    where a.script_run_id = s.script_run_id
                                    and r.app_exec_id = a.app_exec_id
                                    and s.script_run_id = '",input$scriptId,"'
                                    group by a.app_name",sep=""))
    #print(v$data)
    
    cpu$data <- dbGetQuery(con, paste("SELECT app_name, 
                                    100*sum(kernel_secs)/(sum(kernel_secs)+sum(user_secs)) as sys_percent, 
                                    100*sum(user_secs)/(sum(kernel_secs)+sum(user_secs)) as user_percent 
                                    from app_exec natural join resource_usage
                                    where script_run_id='",input$scriptId,"'
                                    group by app_name;",sep=""))
    print(cpu$data)
  })
  
  #Boxes
  output$totalTime <- renderValueBox({
    time <- dbGetQuery(con, paste("select duration from script_run where script_run_id ='",
                                  input$scriptId,"'", sep=""))
    if (is.null(v$data))
      valueBox("0", "Total execution time (secs)", icon("area-chart")) 
    else
      valueBox(round(time, digits = 2), "Total execution time (secs)", icon("area-chart"))
  })
  
  output$totalApps <- renderValueBox({
     app <- dbGetQuery(con, paste("select count(*) from app_exec where script_run_id ='",
                                 input$scriptId,"' group by script_run_id", sep=""))
    if (is.null(v$data))
      valueBox("0","Total apps executed", color = 'yellow',icon("bar-chart"))
    else
      valueBox(app,"Total apps executed", color = 'yellow',icon("bar-chart"))
  })
  
  output$finalState <- renderValueBox({
    final.state <- dbGetQuery(con, paste("select final_state from script_run where script_run_id ='",
                                         input$scriptId,"'", sep=""))
    if (is.null(v$data))
      valueBox("None","Final state", color = "red")
    else
      valueBox(final.state, "Final state", icon("gears"),
               color = if (identical(as.character(final.state), "SUCCESS")) "green" else "red")
  })
  
  #Summary
  output$summaryPlot <- renderPrint({
    summary(v$data)
  })
  
  #Table Query
  output$tableQuery <- renderTable({
    print(data.frame(v$data))
  })
  
  #Table Legend
  output$tableLegend <- renderFormattable({
    if (is.null(v$data)) return()
    apps <- v$data[ ,1]
    #print(apps)
    length.legend <- length(apps)
    #print(length.legend)
    colorLegend <- colours[1:length.legend]
    df <- data.frame(apps, numbers = 1:length.legend, stringsAsFactors=FALSE)
    apps <- paste(df$numbers, apps, sep=". ")
    print(apps)
    tableLegend <- data.frame(apps)
    formattable(tableLegend, list(apps = formatter("span", style = x ~ style(color = colorLegend))))
  })
  
  #Plot Duration
  output$plotDuration <- renderPlot({
    if (is.null(v$data)) return()
    duration <- v$data[ ,2]
    barplot(duration, log = "y", las=2, main = "Apps x Duration", beside=TRUE, col=colours,
            ylab = "Avg Duration Time (secs)", cex.names=0.2)
  })
  
  #Plot CPU
  output$plotCpu <- renderPlot({
    if (is.null(v$data)) return()
    cpu <- v$data[ ,3]
    barplot(cpu, log = "y", las=2, main= "Apps x Cpu usage", beside=TRUE, col=colours,
            ylab = "Avg Cpu usage", cex.names=0.2)
  })
  
  #Plot Bytes Written
  output$plotBytesWritten <- renderPlot({
    if (is.null(v$data)) return()
    bytes.written <- v$data[ ,4]
    barplot(bytes.written, log = "y", las=2, main= "Apps x Written bytes", beside=TRUE, col=colours,
            ylab = "AVG Written Bytes", cex.names=0.2)
  })
  
  #Plot Bytes Read
  output$plotBytesRead <- renderPlot({
    if (is.null(v$data)) return()
    bytes.read <- v$data[ ,5]
    bytes.read <- bytes.read[bytes.read != "0"]
    barplot(bytes.read, log = "y", las=2, main= "Apps x Read bytes", beside=TRUE, col=colours,
            ylab = "AVG Read Bytes", cex.names=0.2)
  })
  
  #Plot Memory
  output$plotMemory <- renderPlot({
    if (is.null(v$data)) return()
    memoria <- v$data[ ,6]
    barplot(memoria,log = "y", las=2, main= "Apps x Memory",beside=TRUE,col=colours,
            ylab = "AVG Used Memory",cex.names=0.5)
    })
  
  output$plotKernelUser <- renderPlot({
    if (is.null(cpu$data)) return()
    subset <- t(data.frame(cpu$data[ ,2], cpu$data[ ,3]))
    
    barplot(subset, names.arg=1:length(cpu$data[ ,1]), legend = c("sys_percent", "user_percent"),
            beside=TRUE, ylim=c(0,100),
            col=c(colours[1:2]), main = "Apps x CPU usage",
            ylab = "Percentage of use", xlab = "Activity (see Legend box above)")
  })
  
  #-----------------------------------------------------------------------------------------------------------------------
  
  #Query Regression
  r.data <- reactive({
    d <- dbGetQuery(con, paste("select s.script_run_id, f.size from staged_in i, file f, app_exec a, script_run s
                                   where f.file_id = i.file_id
                                   and a.app_exec_id = i.app_exec_id
                                   and a.script_run_id = s.script_run_id
                                   and s.script_filename = '",input$scriptName,"'
                                   and i.file_id not in (select o.file_id from staged_out o)", sep = ""))
    d$size <- as.numeric(d$size)
    sum.size <- aggregate(d$size ~ d$script_run_id, d, FUN = function(x){sum(as.numeric(x))})
    colnames(sum.size) <- c("script_run_id", "size")
    
    duration <- dbGetQuery(con, paste("select script_run_id, duration from script_run where script_filename='",input$scriptName, "'", sep = ""))
    
    r.data <- join(sum.size, duration)
  })
  
  indexes <- reactive({ sort(sample(nrow(r.data()), nrow(r.data())*.7)) }) 
  #Training
  train <- reactive({ r.data()[indexes(),] }) 
  #Test
  test <- reactive({ r.data()[-indexes(),] })
  
  #Linear
  model <- reactive({ lm(train()[ ,3] ~ train()[ ,2]) }) #(y~x) duration~size
  #Quadratic
  model.2 <- reactive({ lm(train()[ ,3] ~ poly(train()[ ,2], 2, raw=TRUE), train()) })
  
  
  #Plot Regression
  output$plotRegression <- renderPlot({
   plot.train = plot(train()[ ,2], train()[ ,3], xlab="Input size", ylab="Total execution time (s)", pch = 20, 
                       xlim=range(train()[ ,2], train()[ ,2]))
   points(test()[ ,2], test()[ ,3], pch = 20, col = "red")
   abline(model(), col=colours[1])
   curve(coefficients(model.2())[1] + coefficients(model.2())[2]*x + coefficients(model.2())[3]*x*x, add = T, col = colours[2])
   
   #Description
   model.legend = c("Linear", "Quadratic")
   legend("topleft", xpd = TRUE, model.legend, col=colours, lty = 1, cex = 1, bty="n")
  })
  
  #Table Description
  output$tableDescription <- renderFormattable({
    sets <- c("Training", "Testing")
    points <- c(nrow(train()), nrow(test()))
    tableDescription <- data.frame(sets, points)
    formattable(tableDescription, list(sets = formatter("span", style = x ~ style(color = c("black", "red")))))
  })
  
  #Statistics
  output$textDataSummary <- renderTable({
    sum.table <- rbind(t(summary(r.data()[ ,2])), t(summary(r.data()[ ,3])))
  })

  #Estimate 1
  output$executionTime <- renderPrint({
    attach(train())
    m <- lm(duration ~ size, train())
    new <- data.frame(size=as.numeric(input$inputSize))
    time <- predict(m, new, interval = 'predict')
    print(time[1])
  })
  
  #Estimate 2
  output$executionTime2 <- renderPrint({
    attach(train())
    m.2 <- lm(duration ~ poly(size, 2, raw=TRUE), train())
    new <- data.frame(size=as.numeric(input$inputSize))
    time.2 <- predict(m.2, new, interval = 'predict')
    print(time.2[1])
  })
  
  
#  downloadFormattInput <- reactive({
#    switch(input$downloadFormatt,
#           "Duration" = 1,
#           "Cpu Usage"=2,
#           "Written Bytes"=3,
#           "Read Bytes"=4,
#           "Memory used"=5,
#           "All Graphis"=6)
#  })
  
  #Download plot
  output$downloadButton <- downloadHandler(
    filename = function(){
      paste(input$scriptId,input$downloadFormatt,sep=".")
    },
    content = function(file){
      #Duration
      if(input$downloadFormatt == "1"){
        png(file)
      {
        apps <- v$data[ ,1]
        duration <- v$data[ ,2]
        barplot(duration, log = "y", las=2, main = "Apps x Duration", beside=TRUE, col=colours,
                ylab = "Avg Duration Time (secs)", cex.names=0.2)
        legend("topleft", xpd = TRUE, legend = apps, col=colours, lty = 1, cex = 1, bty="n")
      }
      } else
      #Cpu Usage
      if(input$downloadFormatt == "2"){
        png(file)
      {  
        apps <- v$data[ ,1]
        cpu <- v$data[ ,3]
        barplot(cpu, log = "y", las=2, main= "Apps x Cpu usage", beside=TRUE, col=colours,
                ylab = "Avg Cpu usage", cex.names=0.2)
        legend("topleft", xpd = TRUE, legend = apps, col=colours, lty = 1, cex = 1, bty="n")
       }
      } else
      #Written Bytes
      if(input$downloadFormatt == "3"){
        png(file)
      {  
        apps <- v$data[ ,1]
        bytes.written <- v$data[ ,4]
        barplot(bytes.written, log = "y", las=2, main= "Apps x Written bytes", beside=TRUE, col=colours,
                ylab = "AVG Written Bytes", cex.names=0.2)
        legend("topleft", xpd = TRUE, legend = apps, col=colours, lty = 1, cex = 1, bty="n")
      }
      } else
      #Read Bytes
      if(input$downloadFormatt == "4"){
        png(file)
      {  
        apps <- v$data[ ,1]
        bytes.read <- v$data[ ,5]
        bytes.read <- bytes.read[bytes.read != "0"]
        barplot(bytes.read, log = "y", las=2, main= "Apps x Read bytes", beside=TRUE, col=colours,
                ylab = "AVG Read Bytes", cex.names=0.2)
        legend("topleft", xpd = TRUE, legend = apps, col=colours, lty = 1, cex = 1, bty="n")
      }
      } else
      #Memory used
      if(input$downloadFormatt == "5"){
        png(file)
      {  
        apps <- v$data[ ,1]
        memoria <- v$data[ ,6]
        barplot(memoria,log = "y", las=2, main= "Apps x Memory",beside=TRUE,col=colours,
                ylab = "AVG Used Memory",cex.names=0.5)
        legend("topleft", xpd = TRUE, legend = apps, col=colours, lty = 1, cex = 1, bty="n")
      }
      } else
      #All Graphis
      if(input$downloadFormatt == "6"){
      pdf(file)
      {
        apps <- v$data[ ,1]
        bytes <- v$data[ ,4]
        duration <- v$data[ ,2]
        cpu <- v$data[ ,3]
        memoria <- v$data[ ,6]
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
      }
          graphics.off()
    }
  )
  
}


  

