library(shiny)
library(ggplot2)
#library(RPostgreSQL)
library(sqldf)
library(formattable)
library(RColorBrewer)
library(shinydashboard)
library(DT)
library(plyr)
library(dplyr)
library(reshape)
library(lubridate)
library(scales)
library(anytime)
library(shinyjs)

#  ------------------------------------------------------------------------

#colours <- brewer.pal(12,"Paired")
colours <- c(hue_pal(h = c(0, 30000) + 360, c = 110, l = 65, h.start = 10,
                     direction = 1)(40))

#ajuste <- par(mar=c(1,4,3,1)+0.6)

con <- dbConnect(SQLite(), "~/swift_provenance.db")

script_names <- as.vector(dbGetQuery(con, "select distinct script_filename from script_run")[,1])
