 function(input, output, clientData, session) {
  
   runjs('
        var el2 = document.querySelector(".skin-blue");
        el2.className = "skin-blue sidebar-mini";
        ')
   
   
  #Data = NULL to avoid errors
  v <- reactiveValues(data = NULL)
  cpu <- reactiveValues(data = NULL)
  rw <- reactiveValues(data = NULL)
  gantt <- reactiveValues(data = NULL)
  #app_args <- reactiveValues(data = NULL)
  workflow_args <- reactiveValues(data = NULL)
  #ids <- reactiveValues(data=NULL)
  
  observe({
    ids <- reactive({
      dbGetQuery(con, paste("select script_run_id from script_run where script_filename = '",
                            input$scriptName,"' and start_time between '", as.Date(input$dateId[1], "%Y-%m-%d"), "%' and '", 
                            as.Date(input$dateId[2], "%Y-%m-%d"), "%'", sep=""))
    }) #fim reactive
    #print(ids$data)
    updateSelectInput(session, inputId = "scriptId", choices = ids())
  })#fim observe
  
  #Data
  observeEvent(input$updateButton, {
    v$data <- dbGetQuery(con, paste("select  
                                    a.app_name as app,
                                    round(avg(a.duration), 2) as avg_duration,
                                    round(avg(r.percent_cpu),2) as avg_percent_cpu,
                                    round(avg(r.fs_writes),2) as avg_fs_writes,
                                    round(avg(r.fs_reads),2) as avg_fs_reads,
                                    round(avg(r.max_rss),2) as avg_max_rss,
                                    round(100*sum(kernel_secs)/(sum(kernel_secs)+sum(user_secs)),2) as sys_percent, 
                                    round(100*sum(user_secs)/(sum(kernel_secs)+sum(user_secs)),2) as user_percent,
                                    substr(a.start_time, 1, 22) as start,                     
                                    a.duration
                                    from app_exec a, script_run s, resource_usage r
                                    where a.script_run_id = s.script_run_id
                                    and r.app_exec_id = a.app_exec_id
                                    and s.script_run_id = '",input$scriptId,"'
                                    group by app;",sep=""))
    v$data[["id"]] <- c(1:nrow(v$data))
    v$data <- v$data[colnames(v$data)[c(11,1:10)]]
    print(v$data)
    rw$data <- dbGetQuery(con, paste("select * from 
                                      (select 
                                      case when a.app_name in ('samtools') then a.app_name || ' ' || g.app_exec_arg
                                      when a.app_name in ('GenomeAnalysisTK') then a.app_name || ' ' || substr(g.app_exec_arg,3,instr(g.app_exec_arg,' -')-2)
                                      else a.app_name end as app,
                                      (sum(size)/1024/1024) as read 
                                      from file natural join staged_in natural join app_exec a natural join app_exec_argument g  
                                      where script_run_id='",input$scriptId,"'
                                      and g.arg_position = 1
                                      group by app_name)
                                      natural join
                                      (select 
                                      case when a.app_name in ('samtools') then a.app_name || ' ' || g.app_exec_arg
                                      when a.app_name in ('GenomeAnalysisTK') then a.app_name || ' ' || substr(g.app_exec_arg,3,instr(g.app_exec_arg,' -')-2)
                                      else a.app_name end as app,
                                      (sum(size)/1024/1024) as written 
                                      from file natural join staged_out natural join app_exec a natural join app_exec_argument g
                                      where script_run_id='",input$scriptId,"' 
                                      and g.arg_position = 1
                                    
                                      group by app_name);",sep=""))
    print(rw$data)
    
    # app_args$data <- dbGetQuery(con, paste("select app_name, arg_1, arg_2, arg_3, arg_4, arg_5, arg_6 from app_arg 
    #                                   where script_run_id='",input$scriptId,"';",sep=""))
    # #print(app_args$data)
    
    # workflow_args$data <- dbGetQuery(con, paste("select script_filename, ref, libF, libR, gtf, t, op from script_arg 
    #                                   where script_run_id='",input$scriptId,"';",sep=""))
    
    gantt$data <- dbGetQuery(con, paste("select  
                                     case when a.app_name in ('samtools') then a.app_name || ' ' || g.app_exec_arg
                                     when a.app_name in ('GenomeAnalysisTK') then a.app_name || ' ' || substr(g.app_exec_arg,3,instr(g.app_exec_arg,' -')-2)
                                     else a.app_name end as app_name,
                                    substr(a.start_time, 1, 22) as start,                     
                                    a.duration
                                    from app_exec a, script_run s, resource_usage r, app_exec_argument g 
                                    where a.script_run_id = s.script_run_id
                                    and r.app_exec_id = a.app_exec_id
                                    and g.app_exec_id = a.app_exec_id
                                    and g.arg_position = 1
                                    and s.script_run_id = '",input$scriptId,"'
                                    group by a.app_exec_id order by start;",sep=""))
  })
  
  #PROFILER ############################################################################################################
  
  #BOXES
  source('modules/profiler/boxes.R', local = TRUE)
  
  #TABLES
  source('modules/profiler/tables.R', local = TRUE)

  #PLOTS
  source('modules/profiler/plots.R', local=TRUE)
  
  ####################################################################################################################
  
  # #Table Execution Overview (app)
  # output$tableAppsExecution <- renderDataTable({
  #   #print(data.frame(cmd$data))
  #   DT::datatable(data.frame(app_args$data))
  # })
  # 
  # output$tableWorkflowExecution <- renderDataTable({
  #   #print(data.frame(cmd$data))
  #   DT::datatable(data.frame(workflow_args$data), options = list(dom = 'tip'))
  # })
  
  
  #DOWNLOAD ############################################################################################################ 
  
  source('modules/download/plots.R', local = TRUE)
  
}


  

