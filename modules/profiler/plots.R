#Plot Duration
output$plotDuration <- renderPlot({
  if (is.null(v$data)) return()
  df <- data.frame(v$data[ ,c(1,3)])
  df$avg_duration <- df$avg_duration/60
  barplot(df$avg_duration, 
          las=2, 
          beside=TRUE, 
          col=colours, 
          ylim=range(0, max(df$avg_duration)+100),
          ylab="Avg Duration Time (min)", xlab="Apps", 
          cex.names=1, 
          border=NA, 
          names.arg=df$id)
})

#Plot CPU
output$plotCpu <- renderPlot({
  if (is.null(v$data)) return()
  df <- data.frame(v$data[ ,c(1,4)])
  df <- df[!df$avg_percent_cpu == "0", ]
    barplot(df$avg_percent_cpu, 
            las=2,
            log="y", 
            beside=TRUE, 
            col=colours, 
            ylim=c(1, max(df$avg_percent_cpu)),
            ylab="Avg CPU Usage (log)", xlab="Apps", 
            cex.names=1, 
            border=NA, 
            names.arg=df$id)
})

#Plot Bytes Written
output$plotBytesWritten <- renderPlot({
  if (is.null(v$data)) return()
  df <- data.frame(v$data[ ,c(1,5)])
  df <- df[!df$avg_fs_writes == "0", ]
  par(mar=c(5,6,4,2)+0.1)
  barplot(df$avg_fs_writes, 
          las=2,
          log="y", 
          beside=TRUE, 
          col=colours,
          ylab="", xlab="Apps", 
          cex.names=1, 
          border=NA, 
          names.arg=df$id)
  mtext("Avg Written Bytes", side=2, line=5)
})

#Plot Bytes Read
output$plotBytesRead <- renderPlot({
  if (is.null(v$data)) return()
  df <- data.frame(v$data[ ,c(1,6)])
  df <- df[!df$avg_fs_reads == "0", ]
  df$avg_fs_reads <- df$avg_fs_reads
  #print(df)
  par(mar=c(5,6,4,2)+0.1)
  barplot(df$avg_fs_reads, 
          las=2,
          log="y",
          beside=TRUE, 
          col=colours,
          ylab="", xlab="Apps", 
          cex.names=1, 
          border=NA, 
          names.arg=df$id)
  mtext("Avg Read Bytes", side=2, line=5)
})

#Plot Memory
output$plotMemory <- renderPlot({
  if (is.null(v$data)) return()
  df <- data.frame(v$data[ ,c(1,7)])
  par(mar=c(5,6,4,2)+0.1)
  #opt <- options("scipen" = 20)
  barplot(df$avg_max_rss, 
          las=2,
          beside=TRUE, 
          log="y", ylim=c(0.001,10e8),
          col=colours,
          ylab="", xlab="Apps", 
          cex.names=1, 
          border=NA, 
          names.arg=df$id)
  mtext("Avg memory used", side=2, line=5)
  #options(opt)
  #pts <- pretty(df$avg_max_rss / 10000000)
  print(log10(as.numeric(df$avg_rss_max)))
  #axis(2, at = pts, labels = paste(pts, "MM", sep = ""))
})

#Plot Kernel x User
output$plotKernelUser <- renderPlot({
  if (is.null(v$data)) return()
  subset <- t(data.frame(v$data[ ,8], v$data[ ,9]))
  
  barplot(subset, 
          names.arg=v$data[ ,1], 
          legend = c("sys_percent", "user_percent"),
          las=2,
          beside = TRUE,
          ylim=c(0,100),
          col=c(colours[1:2]), 
          border=NA,
          ylab="Percentage of use", xlab="Apps",
          args.legend=list(
            x=ncol(subset),
            bty = "n"
          ))
})

#Plot total read and write
output$plotReadWritten <- renderPlot({
  if (is.null(rw$data)) return()
  #subset <- t(data.frame(v$data[ ,5], v$data[ ,6]))
  aux <- which(rw$data[ ,1] %in% v$data[ ,2])
  #print(v$data[aux, 2])
  subset <- t(data.frame(rw$data[ ,2], rw$data[ ,3]))
  par(mar=c(5,6,4,2)+0.1)
  barplot(subset, 
          names.arg=v$data[aux,1], 
          legend = c("read", "write"),
          las=2,
          beside=TRUE, 
          ylim=c(0,max(subset)),
          col=c(colours[5:6]), 
          ylab = "", xlab="Apps", border = NA)
  mtext("Data (Mb)", side=2, line=5)
})

#Plot Gantt
output$plotGantt <- renderPlot({
  if (is.null(v$data)) return()
  df <- df_original <- gantt$data[,c(1,2,3)] #gantt$data
  df_nrow <- nrow(df)
  
  df$app_name_n <- paste(c(1:df_nrow)) 
  
  df$start <- as.numeric(as.POSIXct(df$start, origin="1970-01-01"))
  df$start_secs <- 0
  
  for (i in 2:df_nrow) {
    df$start_secs[i] <- df$start[i]-df$start[1]
  }
  df$start_secs <- round(df$start_secs, 2)
  df$end <- df$start_secs + df$duration
  #print(df)
  mdf <- mdf_original <- melt(df, measure.vars = c("start_secs", "end"))
  mdf$app_name_n <- factor(mdf_original$app_name_n, levels=(unique(mdf_original[order(mdf$value), "app_name_n"])))
  #print(mdf)
  
  ggplot(mdf, aes(x=value, y=app_name_n, colour=app_name)) +
    # draw the lines
    geom_line(size = 3) +
    # add x and y axis
    xlab("Time") + ylab("Number of executed apps") +
    # make the theme minimal
    theme_minimal() +
    theme(#axis.text.y=element_blank(),
      #legend.position="none"
      axis.title.y=element_text(margin=margin(0,20,0,0), face = "bold"),
      axis.title.x=element_text(margin=margin(20,0,0,0), face = "bold")
    ) +
    scale_y_discrete(name="Number of executed apps", seq(0, nrow(df), by = 200))
})
  
  
#   df$app_name <- paste(c(1:df_nrow), df$app_name, sep='.') 
#   df$start <- as.numeric(as.POSIXct(df$start))
#   df$end <- df$start + df$duration
#   
#   mdf <- mdf_original <- melt(df, measure.vars = c("start", "end"))
#   mdf$app_name <- factor(mdf_original$app_name, levels=(unique(mdf_original[order(mdf$value), "app_name"])))
#   #print(mdf)
#   ggplot(mdf, aes(x=anytime(mdf$value), y=app_name, colour=app_name)) +
#     # draw the lines
#     geom_line(size = 6) +
#     # add x and y axis
#     xlab("Time") + ylab("") +
#     # make the theme minimal
#     theme_minimal() +
#     theme(legend.position="none")
#   
# })