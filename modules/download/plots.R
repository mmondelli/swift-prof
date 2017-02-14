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

