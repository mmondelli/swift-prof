library(shiny)
library(ggplot2)
library(RPostgreSQL)
library(sqldf)
library(plyr)
library(formattable)
library(RColorBrewer)
library(shinydashboard)

colours <- brewer.pal(12,"Paired")

ajuste <- par(mar=c(1,4,3,1)+0.6)

drv <- dbDriver("PostgreSQL")
#con <- dbConnect(SQLite(), "~/provenance_transcript.db")
con <- dbConnect(drv, dbname = "swift_provenance",
                 host = "localhost", port = 5432,user = "postgres", password = 'postgres')

script_names <- as.vector(dbGetQuery(con, "select distinct script_filename from script_run")[,1])
