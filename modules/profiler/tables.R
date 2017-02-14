#Summary
output$summaryPlot <- renderPrint({
  if (is.null(v$data)) return()
  summary(v$data)
})

#Table Query
output$tableQuery <- renderFormattable({
  if (is.null(v$data)) return()
  df <- data.frame(v$data[1:9])
  #print(df)
  formattable(data.frame(df), list(app = 
                                     formatter("span", style = x ~ style(color = colours[1: nrow(v$data)]))))
  #print(data.frame(v$data))
})