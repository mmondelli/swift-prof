#Boxes
output$totalTime <- renderValueBox({
  time <- dbGetQuery(con, paste("select duration from script_run where script_run_id ='",
                                input$scriptId,"'", sep=""))
  if (is.null(v$data))
    valueBox("0", "Total execution time (hours)", icon = icon("clock-o"))
  else
    valueBox(round(((time/60)/60), digits = 2), "Total execution time (hours)", icon = icon("clock-o"))
})

output$totalApps <- renderValueBox({
  app <- dbGetQuery(con, paste("select count(*) from app_exec where script_run_id ='",
                               input$scriptId,"' group by script_run_id", sep=""))
  if (is.null(v$data))
    valueBox("0","Total apps executed", color = 'yellow', icon = icon("cogs"))
  else
    valueBox(app,"Total apps executed", color = 'yellow', icon = icon("cogs"))
})

output$finalState <- renderValueBox({
  final.state <- dbGetQuery(con, paste("select final_state from script_run where script_run_id ='",
                                       input$scriptId,"'", sep=""))
  if (is.null(v$data))
    valueBox("None","Final state", color = "red", icon = icon("close"))
  else
    valueBox(final.state, "Final state",
             color = if (identical(as.character(final.state), "SUCCESS")) "green" else "red",
             icon = if (identical(as.character(final.state), "SUCCESS")) icon("check") else icon("close"))
})