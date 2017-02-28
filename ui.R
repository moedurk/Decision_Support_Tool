library(rhandsontable) #needs to be loaded from ui file, unlike other packages

#ui####

shinyUI(fluidPage(
  titlePanel(strong("Piping Plover Decision Support Tool")),
  tabsetPanel(
    #Introduction####
    tabPanel("Introduction",
             br(),
             sidebarPanel(
               h3 (strong("Purpose")),
               p("To provide site biologists with guidance for deciding whether or not to use
                 nest exclosures for Piping Plover management", style="font-size:14pt"),
               br(),
               h3(strong("Version 1.0, March 2017")),
               p("Developed by Abigail Darrah and Jonathan Cohen, State University of New York College of 
                 Environmental Science and Forestry"),
               br(),
               h3(strong("Getting Started")),
               helpText("Detailed instructions are available to read or download in the", strong("Instructions"),
                        "tab", style="font-size:12pt"),
               br(),
               h4(strong("If you have data:")),
               p("Proceed through the following tabs:"),
               p("1. Upload"),
               p("2. Nest Fate Analysis"),
               p("3. Decision"),
               br(),
               h4(strong("If you do not have data:")),
               p("Skip to",strong("Scenario Modeling"))
             ),#sidebarPanel
             mainPanel(
               img(src = "PIPL.jpg")
             )#mainPanel
    ),
    #Upload tab####
    tabPanel("Upload",
             sidebarPanel(
               wellPanel(
                 fluidRow(
                   column(7,
                          downloadButton("example","Download Example"), #note: feature only works in browser window, not in Rstudio
                          h3(strong("Upload Nest Data File", style="color:#000066")),
                          fileInput("file", label=h4("Load .csv File")),
                          helpText("Click below to arrange data by nest and date, calculate nest check interval length,
                                   and remove data not needed for analysis."),
                          actionButton("clean","Display Cleaned Data"),
                          br(),
                          br(),
                          downloadButton("downloadClean","Download Cleaned Data")
                          ) #column
                 ) #fluidRow
                 ), #wellpanel 
               h3(strong("Summary of Data Entered", style="color:#000066")),
               textOutput("nests"),
               textOutput("hatches"),
               textOutput("preds"),
               textOutput("abans"),
               textOutput("floods"),
               textOutput("uknfail"),
               textOutput("otherfail"),
               textOutput("uknfate"),
               br(),
               wellPanel(
                 h3(strong("Status Code Key"), style="color:#000066"),
                 p("1 = Active", style="font-size:14pt; color:#000066"),
                 p("2 = Depredated", style="font-size:14pt; color:#000066"),
                 p("3 = Tidal Flooding or Weather-related Failure", style="font-size:14pt; color:#000066"),
                 p("4 = Abandoned", style="font-size:14pt; color:#000066"),
                 p("5 = Unknown Cause of Failure", style="font-size:14pt; color:#000066"),
                 p("6 = Hatched", style="font-size:14pt; color:#000066"),
                 p("7 = Unknown Fate (Hatched or Failed)", style="font-size:14pt; color:#000066"),
                 p("8 = Other Cause of Failure (e.g. trampled)", style="font-size:14pt; color:#000066")
               )#wellPanel
               ), #sidebar
             fluidRow(
               rHandsontableOutput("hot")
             )
             
             ),#tabPanel upload
    #Nest Fate Analysis Tab####
    tabPanel("Nest Fate Analysis", 
             fluidRow(
               column(6,
             wellPanel(
               h3("Press button below after uploading nest data file", style="color:#000066"),
               actionButton("analyze.go", "Analyze Nest Fate Data"),
               br(),
               br(),
               helpText("Analysis may take a few minutes. Results will be displayed below once the model has finished running.
                        Probabilities will be displayed as percentages, rounded to the nearest 1 percent,
                        +/- one standard deviation", style="font-size:12pt")
               ) #wellPanel
              ) #column
             ), #fluidRow
             fluidRow(
               h2(strong("Fate Probabilities",style="color:#000066; padding:0px 0px 0px 10px")),
               column(2),
               column(2,
                      h3("With Exclosures",style="color:#000066")
               ),#column
               column(2,
                      h3("Without Exclosures", style="color:#000066")
               )
             ), #fluidRow
             fluidRow(
               column(2,
                      h3("Hatch",style="color:#000066"),
                      h3("Predation", style="color:#000066"),
                      h3("Abandonment",style="color:#000066"),
                      h3("Tide/Weather",style="color:#000066"),
                      br()
               ),
               column(2,
                      h3(textOutput("hatch.ex")),
                      h3(textOutput("pred.ex")),
                      h3(textOutput("aban.ex")),
                      h3(textOutput("flood.ex"))
               ),
               column(2,
                      h3(textOutput("hatch.un")),
                      h3(textOutput("pred.un")),
                      h3(textOutput("aban.un")),
                      h3(textOutput("flood.un"))
               )
             )#fluidRow
               ), #Nest Fate Panel
    #Decision/Pop Tab####
    tabPanel("Decision", 
             fluidRow(
               column(8,
                      wellPanel(
                        h2(strong("Decision Guideline Plots", style="color:#000066")),
                        helpText("The 3D plot below shows the difference in population growth rate with exclosure use as a 
                                 function of site-level nest predation and nest abandonment probabilities, along with an estimate of
                                 where your site falls.  Green values indicate an increase in population growth rate with exclosure
                                 use, while red indicates a decrease in population growth rate with exclosure use compared to no exclosures.
                                 "),
                        plotOutput("ThreeDplot"),
                        br()
                        )
                      )#column
               ), #fluidRow
             fluidRow(
               column(8,
                      wellPanel(
                        h2(strong("Population Growth Rate", style="color:#000066")),
                        helpText("To see your predicted population growth rates with and without
                                 exclosure use, first upload and analyze nest check data, then press the 
                                 button below."),
                        actionButton("button", "Press to Calculate"),
                        br(),
                        plotOutput("lambdaplot")
                        )
                      )
             ),
             fluidRow(column(4,
                     h2(strong("Trajectory Probabilities", style="color:#000066; padding:0px 0px 0px 10px"))
                     )),
             fluidRow(
                      column(2),
                      column(2,
                          h3("With Exclosures",style="color:#000066")
                         ),#column
                      column(2,
                          h3("Without Exclosures", style="color:#000066")
                      )
             ),#fluidRow
             fluidRow(
               column(2,
                      h3("Rapid Growth:",style="color:#000066"),
                      h3("Growth:", style="color:#000066"),
                      h3("Decline:",style="color:#000066"),
                      h3("Rapid Decline:",style="color:#000066"),
                      br()
                     ),
               column(2,
                      h3(textOutput("growthSteepEx")),
                      h3(textOutput("growthEx")),
                      h3(textOutput("declineEx")),
                      h3(textOutput("declineSteepEx"))
                      ),
               column(2,
                      h3(textOutput("growthSteepUn")),
                      h3(textOutput("growthUn")),
                      h3(textOutput("declineUn")),
                      h3(textOutput("declineSteepUn"))
                      )
             ),#fluidRow
             br(),
             br()
          ), #population panel
    #Thresholds tab####
    tabPanel("Abandonment Tolerance",
             fluidRow(
               column(8,
                      wellPanel(
                        h2(strong("Abandonment Tolerance"), style = "color:#000066"),
                        helpText("This module provides guidelines for when to reassess your decision to use exclosures.
                                 This model simulates increasing numbers of abandonments while keeping nest predation risk
                                 constant.The plot below compares the probability of population decline without exclosures
                                 based on your site's current conditions (dashed line) to the probability of population decline 
                                 with increasing numbers of nest abandonments (solid line).", style="font-size:12pt")
                        ))),
             fluidRow(
               h3(strong("Enter Site Characteristcs"), style = "color:#000066; padding: 0px 0px 0px 10px"),
               column(3,
                    numericInput("Pairs", "Starting # Pairs", value=20, min=1, step=1))
              ),
             fluidRow(
               h3(strong("Press to Calculate Threshold"), style = "color:#000066; padding: 0px 0px 0px 10px"),
               column(3,
               actionButton("threshold.data", "Calculate Threshold")
               ) #putting button in a column puts some space between button and left edge of screen 
             ), #fluidrow
             fluidRow(
               column(8,
                      br(),
                      h3(strong("Threshold Plot"), style = "color:#000066"),
                           plotOutput("thresh.abans")
               ) #column
             ), #fluidrow
             fluidRow(
               wellPanel(
                   h3(textOutput("reassess"))
               )
             )#fluidRow
             
       ), #threshold tabPanel
    #Scenario Modeling####
    tabPanel("Scenario Modeling",
             fluidRow(
               column(6,
                wellPanel(
                   h2(strong("Scenario Modeling"), style = "color:#000066"),
                   helpText("This module allows users to explore the effects of hypothetical scenarios based on 
                            user-supplied values of predation rates, abandonment rates, and exclosure-related 
                            adult mortality risk.", style="font-size:12pt")
                   ) #wellPanel
                 ) #column
               ), #fluidRow
             
          h2("Choose Values", style = "color:#000066"),
          uiOutput("resettableScenarioValues"),
            # sliderInput("predRisk", "Predation Risk without Exclosures", min = 0, max = 99, value = 60),
            # sliderInput("abanRisk", "Abandonment Risk with Exclosures", min = 0, max = 99, value = 10),
            # sliderInput("mortality", "Mortality Probability Given Abandonment", min = 0, max = 100, value = 70),
             h3(textOutput("warning"), style="color:red"),
             actionButton("scenario","Press to Calculate"),
            actionButton("reset_input", "Press to Reset Values"),
             fluidRow(
               column(6,
                      plotOutput("scenLambdaPlot")
                      )
             ),
             fluidRow(column(4,
                             h2(strong("Trajectory Probabilities", style="color:#000066; padding:0px 0px 0px 10px"))
             )),
             fluidRow(
               column(2),
               column(2,
                      h3("With Exclosures",style="color:#000066")
               ),#column
               column(2,
                      h3("Without Exclosures", style="color:#000066")
               )
             ),#fluidRow
             fluidRow(
               column(2,
                      h3("Rapid Growth:",style="color:#000066"),
                      h3("Growth:", style="color:#000066"),
                      h3("Decline:",style="color:#000066"),
                      h3("Rapid Decline:",style="color:#000066"),
                      br()
               ),
              column(2,
                      h3(textOutput("SCENgrowthSteepEx")),
                      h3(textOutput("SCENgrowthEx")),
                      h3(textOutput("SCENdeclineEx")),
                      h3(textOutput("SCENdeclineSteepEx"))
               ),
               column(2,
                      h3(textOutput("SCENgrowthSteepUn")),
                      h3(textOutput("SCENgrowthUn")),
                      h3(textOutput("SCENdeclineUn")),
                      h3(textOutput("SCENdeclineSteepUn"))
               ),
               column(2,
                      h3(textOutput("SCENgainSteepGrowth")),
                      h3(textOutput("SCENgainGrowth")),
                      h3(textOutput("SCENgainDecline")),
                      h3(textOutput("SCENgainSteepDecline"))
               )
             ),#fluidRow
            br(),
            br()
    ), #scenario panel
    #Report####
    tabPanel("Generate Report",
             sidebarPanel(
               h2("Choose Data to Include in Report"),
               checkboxInput("NestSum", label="Summary of Data Entered"),
               checkboxInput("NestFate", label="Nest Fate Analysis"),
               checkboxInput("Lambda", label="Decision Plots"),
               checkboxInput("Thresholds", label="Abandonment Tolerance"),
               checkboxInput("IncludeScenario", label = "Scenario Modeling")
             ),
             br(),
      downloadButton("Report", "Generate Report")
    ), #report panel
    tabPanel("Instructions", 
             h2("Under Construction")
    )#intructions panel
    
               )#tabsetPanel
             )) #shinyUI
