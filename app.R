library(dplyr)
library(simmer)
library(simmer.plot)
library(ggplot2)
library(shiny)
library(bslib)
library(rsconnect)

#The fluid page defines the UI)
ui <- fluidPage( theme = bs_theme(version = 4,bootswatch = "flatly"),
                 
                 titlePanel(tags$h3("Appointment schedule simulator")),
                 p("Try to emulate a real-life procedure in a safe  and digital environment. This simulation visualizes the process of appointment scheduling with multiple arrivals at a single day. The process involves potential arrivals of Scheduled
        , Unscheduled and Emergency patients. The user can manipulate the input parameters and see their influence on a set of KPI's in the outputs. The purpose of the the tool is to create more intuition 
        about the concept appointment scheduling. Please read the operators manual and the overiew & example for more detailed information.
        
        
          "),
                 hr(),
                 
                 wellPanel(
                         fluidRow(
                                 column(2,
                                        checkboxInput("AllowUAR", "Allow for Unscheduled patients to arrive", value = T)),
                                 column(2,
                                        checkboxInput("AllowEAR", "Allow for Emergency patients to arrive", value = T)),
                                 column(2,
                                        checkboxInput("preempt", "Allow for Emergency patients to preempt", value = T)),
                                 column(2,
                                        checkboxInput("priority", "Allow for priority of Scheduled patients over Unscheduled patients in queue", value = F)),
                                 column(2,
                                        checkboxInput("Renege2", "Allow Unscheduled patients to renege", value = F)),
                                 column(2,
                                        checkboxInput("Seed", "Play around with the same variables", value = F)),
                         ),
                         hr(),
                         fluidRow(
                                 column(4,
                                        sliderInput("CTscanAmount","Amount of CT-scan(s):", min = 1, max = 3,value = 1,step= 1),
                                        sliderInput("SlotAmount","Amount of appointment slots (Scheduled only) to open:", min = 1, max = 3,value = 1,step= 1),
                                        sliderInput("RunFor","Amounts of minutes for simulation to Run:", min = 200, max = 600,value = 320 , step= 5)
                                 ),
                                 column(4,
                                        sliderInput("ManipulateSAR",  "Inter arrival times Scheduled patients:", min = 2, max = 8, value = 5),
                                        sliderInput("ManipulateUAR", "Inter arrival times Unscheduled patients:",min = 5, max = 50 ,value = 15),
                                        sliderInput("ManipulateEAR", "Inter arrival times Emergency patients:",min = 50, max = 150,value = 100)
                                 ),
                                 column(4,fluidRow(
                                         numericInput("HlineAccess","Hline max t acces time:", 0,min=0),
                                         numericInput("HlineWait","Hline max t waiting time:", 0,min=0),
                                         numericInput("WaitingRoom","Hline max n waiting room:", min = 0, max = 10,value = 0,step= 1)
                                 ),
                                 sliderInput("patience", "Amount of Unscheduled patients allowed in queue:",min = 1, max = 10,value = 3, step = 1), 
                                 sliderInput("noshow", "No-show probability of Scheduled patients:",min = 0.00 , max = 0.25,value = 0.05, step= 0.01),
                                 sliderInput("Renege", "Renege of Unscheduled patients after n slots",min = 0 , max = 20,value = 0, step= 1)
                                 )
                         ),
                         
                         p(actionButton("run", "Run the simulation"))
                 ),
                 tabsetPanel(
                         tabPanel("Main interface", (
                                 div(
                                         fluidRow(
                                                 column(6,plotOutput("Accestime")),
                                                 column(6,plotOutput("Waitingtime"))
                                         ),
                                         fluidRow(
                                                 column(4,plotOutput("util")),
                                                 column(4,plotOutput("usage2")),
                                                 column(4,plotOutput("usage"))
                                         ),
                                         fluidRow(
                                                 column(12,verbatimTextOutput("text", placeholder = T))
                                         )
                                 )
                         )
                         ),
                         
                         tabPanel("Finished Patients statistics", (
                                 div(
                                         fluidRow(
                                                 column(6,dataTableOutput("dataframeAcces")),
                                                 column(6,dataTableOutput("dataframeWaiting"))
                                         ),
                                 )
                         )
                         ),
                         
                         tabPanel("All Patients statistics", (
                                 div(
                                         fluidRow(
                                                 column(6,dataTableOutput("dataframeAccessAll")),
                                                 column(6,dataTableOutput("dataframeWaitingAll"))
                                         ),
                                 )
                         )
                         ),
                         
                         tabPanel("Simulation overview & example", (
                                 div(
                                         fluidRow(column(12,tags$img(src ="ModelOverview.jpg"))
                                         ),
                                 )
                         )
                         ),
                         tabPanel("Operators Manual", (
                                 div(
                                         fluidRow(column(12,tags$img(src ="OperatorsManual.jpg"))
                                         ),
                                 )
                         )
                         )
                         
                 )
)


#The server defines the reactive environment, it contains the model and its reactivity
server <- function(input, output) {
        sim <- eventReactive(input$run,{
                
                if(input$Seed){(set.seed(1))} #if put to TRUE > we play with the same variables
                
                
                ##constants## They respond to the input parameters and define arrivalrates, queue sizes etc.
                Manipulate_SAR <- function() {rexp(1,1/input$ManipulateSAR)} 
                
                Allow_UAR <- function() {  
                        if(input$AllowUAR == TRUE) {
                                rexp(1,1/input$ManipulateUAR)
                        } else {(100000000)
                        }
                }
                Allow_EAR <- function() {  
                        if(input$AllowEAR == TRUE) {
                                rexp(1,1/input$ManipulateEAR)
                        } else {(100000000)
                        }
                }
                
                
                Slot_Amount <- function(){as.numeric(input$SlotAmount)}
                CT_Amount <- function() {as.numeric(input$CTscanAmount)}
                
                
                
                Preemptive <- function() {
                        if(input$preempt == T){(4)}
                        else{(3)}
                }
                PrioritySwitch <- function() {
                        if(input$priority == T){(2)}
                        else{(1)}
                }
                Queuesize <- (input$patience)
                
                Renege <- function() { if(input$Renege2 == T){(input$Renege*15)}
                        else{(10000000)}
                }
                
                
                ###trajectory###
                
                patientSAR <- trajectory("Scheduled Patient") %>%               
                        ## The trajectory at the waiting list (SAR only).
                        log_("Appointment is scheduled for patient") %>%               ## txt msg when patient is generated
                        seize("Waiting list", 1) %>%                                   ## 1 appointment by as single patient can be seized at a time 
                        timeout(15) %>%                                                ## The appointment slots are fixed at 15 minutes for every slot opened by resource
                        release("Waiting list", 1) %>%                                 ## Leaving the appointment slot
                        log_("Time for scheduled appointment")%>%                      ## log when patient leaves the waiting list
                        
                        ## $Waiting_list %>% The access_time is: End_time - (start_time + activity time)
                        
                        
                        ## The trajectory of SAR at the CT-scan.
                        set_queue_size("CT-Scan", 1, mod="+") %>%                                                               ##Increase queue size by 1 because SAR does not influence queue size policy
                        leave(prob=function(){runif(1,input$noshow,input$noshow)}, out = trajectory("Reneging patient2") %>%    ##No show behaviour 
                                      set_queue_size("CT-Scan", -1, mod="+")%>% 
                                      log_("Scheduled patient does not show up")) %>%
                        seize("CT-Scan", continue = F, reject = trajectory("Balked patient")%>%                                 ##Siezing CT-scan if possible since queue length max is 3 (or 4).
                                      log_("No-show")%>% set_queue_size("CT-Scan", -1, mod="+"))%>%
                        set_queue_size("CT-Scan", -1, mod="+")%>%                                                               ##decreasing queue size by 1
                        log_("Scheduled patient goes into CT-scan")   %>%               
                        timeout(function() rnorm(1, 15)) %>%                                                                    ## The CT-scan duration takes a norm distr of n=1, mean:15 min and sd: 1.
                        release("CT-Scan", 1)%>%    
                        log_("Finished scheduled visit")                                                                        ## Last log indicating finishing of SAR. 
                
                
                
                patientUAR <- trajectory("Unscheduled Patient") %>%
                        ## The trajectory at the CT-scan                                                
                        log_("Walk-in appointment requested") %>%
                        renege_in(Renege, keep_seized = T ,out =trajectory("Renege patients")%>%             ##Reneging if the queue takes too long, patience is defined in input parameters.
                                          log_("Unscheduled patient is not willing to wait this long"))%>%
                        seize("CT-Scan", continue = F, reject = trajectory("Balked patient")%>%              ##Siezing CT-scan if possible since queue length max is defined in input parameters.
                                      log_("No, the queue is too full"))%>%
                        renege_abort() %>%
                        log_("Unscheduled patient can make use of CT-scan") %>%
                        timeout(function() rnorm(1, 15)) %>%
                        release("CT-Scan", 1)%>%
                        log_("Finished on walk-in basis") 
                
                
                
                patientEmergency <- trajectory("Emergency Patient") %>%                                ## Idem for above 
                        log_("Emergency patient request CT-Scan") %>%  
                        set_queue_size("CT-Scan", 1, mod="+") %>%                               
                        seize("CT-Scan") %>%                                    
                        timeout(function() rnorm(1, 15)) %>%
                        release("CT-Scan", 1)%>%
                        set_queue_size("CT-Scan", -1, mod="+")%>%
                        log_("Finished the emergency visit") 
                
                
                
                ######################SIMULATION ENVIRONMENT######################
                #######################resources&generators#######################
                
                
                Hospital <- simmer("Hospital") %>%                                                    ## The simmer environment that generates the simulation
                        add_resource("Waiting list", 
                                     capacity = Slot_Amount()) %>%                                    ## one waiting list that is a resource and can be seized by the slot amount to open defined in the input parameters
                        
                        add_resource("CT-Scan", capacity = CT_Amount(),                               ## CT-scan that is a resource and can be seized by amount of CT-scans available
                                     queue_size = Queuesize, preemptive = TRUE,                       ## CT-scan has a max. queue size and enables preemptive.
                                     queue_priority = 1,                                              ## Priority is given and preemption occurs LIFO. 
                                     preempt_order = "lifo", queue_size_strict= F)  %>%               ## queue size is not strict so, no resources will be released if queue size is met.
                        
                        add_generator("Scheduled Patient",      
                                      patientSAR, 
                                      (Manipulate_SAR), priority = PrioritySwitch(),
                                      preemptible = 3, restart = TRUE) %>%                            ## SAR generating every n min (rexp)
                        
                        add_generator("Unscheduled Patient", 
                                      patientUAR, 
                                      Allow_UAR,  priority = 1,                                       ## UAR generating every n min (rexp). 
                                      preemptible = 3, restart = TRUE) %>%
                        
                        add_generator("Emergency Patient", 
                                      patientEmergency, 
                                      Allow_EAR,                                                      ## EAR generating every n min (rexp)
                                      priority = Preemptive())                                        ## Indicating priority over other generators, possible preemptive.
                
                run(Hospital,until=(input$RunFor))
                
        })
        output$Accestime <- renderPlot({
                ## Defining the data frame
                df1<-get_mon_arrivals(sim(), per_resource = T)%>%
                        subset(resource == "Waiting list") %>%
                        transform(access_time = end_time + activity_time - start_time)
                ## Defining the plot
                ggplot(df1, mapping = aes(x=end_time,y=access_time, color = sapply(strsplit(name, " "), "[[", 1)))+geom_smooth(method = "lm", formula = y ~ poly(x, 3), se = T)+geom_point()+ geom_hline(yintercept = mean(df1$access_time), color="blue") +
                        annotate(geom="text", x = 0 , 
                                 y = 1.10*mean(df1$access_time),fontface =2, color = "black" , label = as.integer(mean(df1$access_time))) + labs(color='Type of Patients', title= "Plot for access time before entering the institution") + geom_hline(yintercept = if(input$HlineAccess == 0){NULL}
                                                                                                                                                                                                                                                       else {(input$HlineAccess)}, color="red",linetype = "dashed")
        })
        
        output$Waitingtime <- renderPlot({
                
                df2 <-get_mon_arrivals(sim(), per_resource = T)%>%
                        subset(resource == "CT-Scan") %>%
                        subset(activity_time > 0) %>%
                        transform(waiting_time = end_time - start_time - activity_time)
                
                ggplot(df2 , mapping = aes(x=end_time,y=waiting_time, color = sapply(strsplit(name, " "), "[[", 1)))+geom_point()+ geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = T) + geom_hline(yintercept = mean(df2$waiting_time), color="blue") + 
                        annotate(geom="text", x = 0 , fontface =2, color = "black",
                                 y = 1.10*mean(df2$waiting_time) , label = as.integer(mean(df2$waiting_time))) + 
                        labs(color='Type of Patients', title= "Plot for waiting time in waiting room") + geom_hline(yintercept = if(input$HlineWait == 0){NULL}
                                                                                                                    else {(input$HlineWait)}, color="red", linetype = "dashed")
        })
        
        
        output$dataframeWaiting <- renderDataTable({ 
                df3 <- get_mon_arrivals(sim(), per_resource = T)%>%
                        subset(resource == "CT-Scan") %>%
                        subset(activity_time > 0) %>%
                        transform(waiting_time = end_time - start_time - activity_time)
                data.frame(df3)
                
        })
        output$dataframeAcces <- renderDataTable({ 
                df4 <- get_mon_arrivals(sim(), per_resource = T)%>%
                        subset(resource == "Waiting list") %>%
                        transform(access_time = end_time + activity_time - start_time) 
                data.frame(df4)
                
                
        })
        
        output$dataframeWaitingAll <- renderDataTable({ 
                df5 <- get_mon_arrivals(sim(), ongoing=T,per_resource = T)%>%
                        subset(resource == "CT-Scan") %>%
                        transform(waiting_time = end_time - start_time - activity_time)
                data.frame(df5)
                
        })
        output$dataframeAccessAll <- renderDataTable({ 
                df7 <- get_mon_arrivals(sim(), ongoing=T, per_resource = T)%>%
                        subset(resource == "Waiting list") %>%
                        transform(access_time = end_time + activity_time - start_time) 
                data.frame(distinct(df7,name, .keep_all= T))
                
        })
        
        output$text <- renderPrint({
                print(sim())
        })
        
        output$util <- renderPlot({ 
                plot(get_mon_resources(sim()), metric = "utilization")
                
        })
        
        output$usage <- renderPlot({
                plot(get_mon_resources(sim()),"CT-Scan", metric = c("usage"), items= "queue", steps = F) + geom_hline(yintercept = if(input$WaitingRoom == 0){NULL}
                                                                                                                      else {(input$WaitingRoom)}, color="red", linetype = "dashed")
        })
        output$usage2 <- renderPlot({
                plot(get_mon_resources(sim()),"Waiting list", metric = c("usage"), items= "queue", steps = F)
        })
        
}

shinyApp(ui = ui, server = server)
