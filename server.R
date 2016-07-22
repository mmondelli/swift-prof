function(input, output, clientData, session) {

  #Data = NULL to avoid errors
  v <- reactiveValues(data = NULL)

  observe({
    ids <- reactive({
      dbGetQuery(con, paste("select script_run_id from script_run where script_filename = '",
                            input$scriptName,"'", sep=""))
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
  })

  #Boxes
  output$totalTime <- renderValueBox({
    if (is.null(v$data)) return()
    time <- dbGetQuery(con, paste("select duration from script_run where script_run_id ='",
                                  input$scriptId,"'", sep=""))
    valueBox(time, "Total execution time (secs)", icon("area-chart"))
  })

  output$totalApps <- renderValueBox({
    if (is.null(v$data)) return()
    app <- dbGetQuery(con, paste("select count(*) from app_exec where script_run_id ='",
                                 input$scriptId,"' group by script_run_id", sep=""))
    valueBox(app,"Total apps executed", color = 'yellow',icon("bar-chart"))
  })

  output$finalState <- renderValueBox({
    if (is.null(v$data)) return()
    final.state <- dbGetQuery(con, paste("select final_state from script_run where script_run_id ='",
                                         input$scriptId,"'", sep=""))
    valueBox(final.state, "Final state",
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
    print(apps)
    length.legend <- length(apps)
    #print(length.legend)
    colorLegend <- colours[1:length.legend]
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

  #Plot Regression
  output$plotRegression <- renderPlot({
    script_name <- dbGetQuery(con, paste("select script_filename from script_run where script_run_id='",
                                             input$scriptId,"'", sep = ""))

    #Query Regression
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
  output$downloadButton <- downloadHandler(
    filename = function(){
      paste(input$scriptId,input$var3,sep=".")
    },
    content = function(file){
      if(input$var3 == "png")
        png(file)
      else
        pdf(file)

      {
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

      dbDisconnect(con)
      dev.off()
      }
    )
}
