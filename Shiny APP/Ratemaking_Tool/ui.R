#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

load("C:/Users/demcn/Desktop/Ratemaking APP/stateData.RData")

shinyUI(navbarPage("Ratemaking Tool",
  tabPanel("About",
           fluidPage(
        mainPanel(textOutput("appAbout")),
        checkboxInput('question', 'Do you like Math Jax?', FALSE),
        withMathJax(),
        uiOutput('mj'),
        shiny::actionButton(inputId='ab1', label="Learn More", 
                            icon = icon("th"), 
                            onclick ="window.open('https://www.google.com/?gws_rd=ssl#spf=1576254108882', '_blank')")
  )),
  

  
  tabPanel("One-Way Analysis",
  fluidPage(
  # Sidebar with options for the data set
  sidebarLayout(
    sidebarPanel(
      selectizeInput("var", "Variable Selection", choices = c(Source = "SourceOfBusinessCode", Zip = "ZipCode",
                                                              Program = "RatingProgramCode",
                                                              Tenure = "TenureYears",MarketingType = "MarketingTypeCode",
                                                              Membership="MembershipDiscountTypeCode",InsuranceScore = "InsuranceScoreTypeCode",
                                                              MultiProduct="MultiProductCode",PayType="PaymentTypeCode",
                                                              Channel = "ChannelCode",
                                                              PriorCarrier="PriorCarrierCode",ClaimsFree = "ClaimsFreeInd"))

    ),
    # Show a plot of the generated distribution
    mainPanel(
       plotOutput("varPlot", dblclick = "varPlot_dblclick",
                  brush = brushOpts(
                    id="varPlot_brush",
                    resetOnNew = TRUE
                  )),
       tableOutput("tableSum")
    )
  )
)),

tabPanel("Cluster Analysis",           
         fluidPage(
           sidebarLayout(
             sidebarPanel(
               selectInput('clus1','Select First Variable', 
                           selected = "Membership_Type",
                           choices = c(Membership = "Membership_Type",Payment ="Payment_Type",
                                       MaritalStatus ="drvr_marital_status",
                                       NFA= "NFA_CODE",Location_PD ="veh_PDEMLC_RB_20",Location_COLL="veh_collEMLC_RB_20",
                                       Education = "drvr_Education",
                                       StudentInfo= "drvr_Student_Info",
                                       MultiProduct="pol_multiple_product",Tenure = "pol_tenure",Credit ="CCS_psps5078_G50"), 
                           multiple = FALSE,
                           selectize=FALSE),
               
               selectInput('clus2','Select Second Variable', 
                           selected = "Payment_Type",
                           choices = c(Membership = "Membership_Type",Payment ="Payment_Type",
                                       MaritalStatus ="drvr_marital_status",
                                       NFA= "NFA_CODE",Location_PD ="veh_PDEMLC_RB_20",Location_COLL="veh_collEMLC_RB_20",
                                       Education = "drvr_Education",
                                       StudentInfo= "drvr_Student_Info",
                                       MultiProduct="pol_multiple_product",Tenure = "pol_tenure",Credit ="CCS_psps5078_G50"), 
                           multiple = FALSE,
                           selectize=FALSE)
             ),
             mainPanel(
               plotOutput("clusPlot")
             )
           )
         )),

  tabPanel("Models",
           fluidPage(
           sidebarLayout(
             sidebarPanel(
             radioButtons("modOps", "Choose what type of model:", c(
                            "Generalized Linear Model (glm)"= "glmOp",
                            "Gradient Boosted Tree"="gbmOp")),
             
             selectInput('state', 'Filter for State in Table', c(Oregon = "OR","New York" = "NY"),
                        selectize=TRUE),
             
             selectInput('in1','Select Variables', 
                         selected = "Membership_Type",
                         choices = c(Membership = "Membership_Type",Payment ="Payment_Type",
                                             MaritalStatus ="drvr_marital_status",
                                             NFA= "NFA_CODE",Location_PD ="veh_PDEMLC_RB_20",Location_COLL="veh_collEMLC_RB_20",
                                             Education = "drvr_Education",
                                             StudentInfo= "drvr_Student_Info",
                                             MultiProduct="pol_multiple_product",Tenure = "pol_tenure",Credit ="CCS_psps5078_G50"), 
                         multiple = TRUE,
                         selectize=FALSE),
             
            verbatimTextOutput('out1')
            ),
            
             mainPanel(
               tabsetPanel(type="tabs",
                  tabPanel("Summary",verbatimTextOutput("glm")),
                  tabPanel("Plot", plotOutput("modPlot"),
                           downloadButton("downloadPlot","Download Plot")),
                  tabPanel("Table",DT::dataTableOutput("table"),
                           downloadButton("download","Download Data"))
                           )
             )
           )
  )),
tabPanel("Prediction",
         fluidPage(
           # Sidebar with options for the data set
           sidebarLayout(
             sidebarPanel(
               selectizeInput("membership", "Membership", choices = unique(stateData$Membership_Type)),
               selectizeInput("payment", "Payment", choices = unique(stateData$Payment_Type)),
               selectizeInput("marital", "Marital Status", choices = unique(stateData$drvr_marital_status)),
               selectizeInput("nfa", "NFA Code", choices = unique(stateData$NFA_CODE)),
               selectizeInput("collLoc", "COLL Location Score", choices = unique(stateData$veh_collEMLC_RB_20)),
               selectizeInput("pdLoc", "PD Location Score", choices = unique(stateData$veh_PDEMLC_RB_20)),
               selectizeInput("student", "Student Info", choices = unique(stateData$drvr_Student_Info)),
               selectizeInput("education", "Education", choices = unique(stateData$drvr_Education)),
               selectizeInput("multi", "Multi Product", choices = unique(stateData$pol_multiple_product)),
               selectizeInput("tenure", "Tenure", choices = unique(stateData$pol_tenure)),
               selectizeInput("credit", "Credit", choices = unique(stateData$EQFXCCS_psps5078))
           ),
             # Show a plot of the generated distribution
             mainPanel(
               verbatimTextOutput("info"),
               verbatimTextOutput("polPred")
             )
           )
         )
)
))


