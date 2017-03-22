library(rhandsontable) #needs to be loaded from ui file, unlike other packages

#ui####

shinyUI(fluidPage(
  titlePanel("Piping Plover Decision Support Tool"),
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
               p("1. Upload Data"),
               p("2. Decision Support"),
               br(),
               h4(strong("If you do not have data:")),
               p("Skip to",strong("Scenario Modeling"))
             ),#sidebarPanel
             mainPanel(
               img(src = "PIPL.jpg")
             )#mainPanel
    ),
    #Upload Data tab####
    tabPanel("Upload Data",
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
    #Decision Support Tab####
    tabPanel("Decision Support", 
             h1(strong("Nest Fate Analysis"), style="color:#000066"),
             fluidRow(
               column(6,
             wellPanel(
               h3("Press button below after uploading nest data file", style="color:#000066"),
               actionButton("analyze.go", "Analyze Nest Fate Data"),
               br(),
               br(),
               helpText("Analysis may take a few minutes. Results will be displayed below once the model has finished running.
                        Probabilities will be displayed as percentages, rounded to the nearest 1 percent,
                       with standard error in parentheses", style="font-size:12pt")
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
             ),#fluidRow
             br(),
             br(),
         #Decision Support Population Modeling####
            h1(strong("Decision Support Population Modeling"), style="color:#000066"),
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
                      h3("Any Growth:", style="color:#000066"),
                      h3("Any Decline:",style="color:#000066"),
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
             br(),
         #Abandonment Tolerance####
         h1(strong("Abandonment Tolerance"), style="color:#000066"),
         fluidRow(
           column(8,
                  wellPanel(
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
         ),#fluidRow
         br(),
         downloadButton("Report", "Generate HTML Report"),
         downloadButton("ReportWord", "Download Report in MS Word"),
         br(),
         br()
      ), #Decision Analysis Panel
 #threshold tabPanel
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
        fluidRow(
          column(3,
          wellPanel(
          h2("Choose Values", style = "color:#000066"),
          uiOutput("resettableScenarioValues"),
             h3(textOutput("warning"), style="color:red"),
             actionButton("scenario","Press to Calculate"),
            actionButton("reset_input", "Press to Reset Values")
          ) #wellPanel
          ),#column
          column(5,
                 plotOutput("scenLambdaPlot")
           )#column
          ), #fluidRow
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
                      h3("Any Growth:", style="color:#000066"),
                      h3("Any Decline:",style="color:#000066"),
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
               )
             ),#fluidRow
        br(),
        br(),
        #Abandonment Tolerance for Scenario####
        h2(strong("Abandonment Tolerance"),style="color:#000066"),
            br(),
            br(),
        fluidRow(
          h3(strong("Enter Site Characteristcs"), style = "color:#000066; padding: 0px 0px 0px 10px"),
          column(3,
                 numericInput("ScenPairs", "Starting # Pairs", value=20, min=1, step=1))
        ),
        fluidRow(
          h3(strong("Press to Calculate Threshold"), style = "color:#000066; padding: 0px 0px 0px 10px"),
          column(3,
                 actionButton("Scen.threshold.data", "Calculate Threshold")
          ) #putting button in a column puts some space between button and left edge of screen 
        ), #fluidrow
        fluidRow(
          column(8,
                 br(),
                 h3(strong("Threshold Plot"), style = "color:#000066"),
                 plotOutput("scen.thresh.abans")
          ) #column
        ), #fluidrow
        fluidRow(
          wellPanel(
            h3(textOutput("SCENreassess"))
          )
        ),#fluidRow
        downloadButton("ScenReport", "Generate HTML Report"),
        downloadButton("ScenReportWord", "Download Report in MS Word"),
        br(),
        br()
    ), #scenario panel
 #Instructions####
    tabPanel("Instructions", 
             h2("Introduction"),
             p("Nest exclosures are frequently used to increase hatch success of threatened and 
                 endangered populations of Piping Plover (",span(em("Charadrius melodus")),"). However, exclosure use can result 
               in adult mortality, which is not always detected and may result only in apparent nest abandonment. 
               Exclosure use thus increases nest success but carries an unquantified cost in adult survival rates, 
               with unknown consequences for population growth rate. Modeling efforts based on 1312 nests from 
               the U.S. Atlantic Coast revealed unpredictable spatial and temporal variation in nest abandonment 
               (and thus mortality) probability with exclosure use, thus the decision to use exclosures needs to be 
               made on a site-by-site and year-by-year basis."),
             h3("Purpose"),
             p("To help site managers make early- and mid-season decisions about exclosure use for Piping Plover conservation."),
             h3("How It Works"),
             p("This tool accepts nest fate data from the user, and analyzes the data to provide site-specific 
               estimates of exclosure effects and nest abandonment and predation probabilities. These values are then 
               used to predict season-long productivity and breeding adult survival rates, which are combined with 
               published or expert opinion values of other demographic rates to estimate population growth rate at the 
               user’s site with and without exclosures. The results can support a decision to use exclosures or not, 
               based on whether exclosures increase or decrease population growth rate compared to not using exclosures."),
             h3("Model Assumptions and Limitations"),
             p("The models underlying this tool were parameterized using data from the Atlantic Coast population 
               of Piping Plovers, and thus might not be strictly applicable to Great Lakes or Great Plains populations.
               The model assumes that all nest fates have been identified correctly; uncertain fates are best recorded 
               as unknown rather than given a best guess, which could bias your results and affect the decision. 
               The population growth rates are calculated using average survival and fledgling estimates from the 
               literature or expert opinion; if your site or region experiences different vital rates, this may affect 
               the accuracy of the population growth estimates but will not affect whether using exclosures is better 
               than not using exclosures."),
             h2("Overview of Steps"),
             p("Below is a quick reference outlining the steps to take when using this tool. Detailed instructions for
               each step are provided in the remainder of this document."),
             h3("1. Decide when and where to start"),
             p("We recommend using this tool twice during the breeding season: once at the beginning to make an 
               initial management decision based on the previous year’s data, and again after June 1 or after you 
               have accumulated new data for at least 10 complete nests (complete meaning the nests have hatched or 
               failed). If you have data, start in the", span("Upload Data", style="color:blue"), "tab to load your data into the tool and move on to 
               the next Step. If you do not have data, and/or you wish to explore hypothetical situations, you start 
               in the", span("Scenario Modeling", style="color:blue"),"tab."),
             h3("2. Upload Data"),
             p("Using the features in the", span("Upload Data", style="color:blue"), "tab, load and double-check your 
               nest fate data."),
             h3("3. Nest Fate Analysis"),
             p("The first step in the", span("Decision Support", style="color:blue"),"tab analyzes your data to provide estimates of 
               nest hatch, abandonment, predation, and tidal- or weather-caused failure probabilities for exclosed 
               and unexclosed nests."),
             h3("4. Decision Support Population Modeling"),
             p("The second section of the", span("Decision Support", style="color:blue"),"tab uses the estimates 
               from your nest fate analysis and provides predicted population growth rates for your site with and 
               without exclosures."),
             h3("5. Abandonment Tolerance"),
             p("The Abandonment Tolerance section in the", span("Decision Support", style="color:blue"),"tab is an 
               optional model that estimates how many exclosure-related abandonments you can tolerate before 
               reassessing or removing your exclosures."),
             h3("6. Scenario Modeling"),
             p("The", span("Scenario Modeling", style="color:blue"),"section allows users to choose different
               values of predation, abandonment, and adult mortality risk, and to see the predicted outcomes of 
               exclosure use in these hypothetical scenarios.  It is useful when there are limited or no pre-existing 
               data."),
             h3("7. Generate Report"),
             p("In both the", span("Decision Support", style="color:blue"),"and", 
               span("Scenario Modeling", style="color:blue"),"sections include buttons that will generate either 
               HTML or Word document reports summarizing the user’s analyses. "),
             h2("Getting your data into the tool"),
             p("To begin your analysis, navigate to the", span("Upload Data", style="color:blue"),"tab of the tool"),
             img(src = "uploadOverview.png"),
             h3("Sample Size Requirements"),
             p("The tool will run regardless of sample size, but we recommend a minimum of 10 nests be uploaded for 
               getting reliable site- and year-specific recommendations. This sample size cutoff is based on 
               preliminary simulations to calculate the probability of making the correct decision as a function of 
               sample size. With 5 nests, the probability of making the correct decision is 55% (i.e. you might as 
               well flip a coin). With 10 nests the probability is 80%, and the probability continues to increase as 
               sample size gets larger. If you do not have at least 10 COMPLETE nests (i.e. nests that have hatched or
               failed), we recommend skipping the data upload and analysis and moving straight
               to", span("Scenario Modeling", style="color:blue"),"."),
             h3("Data Format Requirements"),
             p("The data file MUST be in a .csv format. If you are unfamiliar with this file type or how to convert
               your data to a .csv format, please refer to Appendix 1 in the circulated pdf instructions, or 
               download the example file and enter or paste your data into it."),
             p("At the top of the sidebar is a button called 
               Download Example, which will download an example data file with the required columns and formatting. 
               Each row contains data from one visit to one nest, with a nest ID label, date or nest check, 
               nest status, and whether or not the nest was exclosed."),
             br(),
             p(strong("IMPORTANT: the Exclosed column refers to whether or not the nest had an exclosure throughout 
                      the PREVIOUS interval. ")),
             br(),
             p("This is because the model links the current nest fate to factors that were present between now and 
               the last time the nest was checked. Therefore, on the day that the nest is exclosed, the entry should 
               be N for that visit and Y for subsequent visits. Failure to follow this guideline will not result 
               in model failure, but it will result in inaccurate estimates of exclosure effects on nest fate."),
             p("The file MUST contain the following columns, formatted as written here: Nest.ID, Date, Status, and 
               Exclosed. The columns can be in any order, and other columns are allowed but will be ignored in the 
               analysis. Entries in the Date column must be in the format mm/dd/yyyy; other formats will produce 
               errors or incorrect date assignment. Exclosed must be Y or N (not case sensitive).  Nest.ID can 
               be any character string, but care must be taken to ensure that each nest name is recorded EXACTLY the 
               same way each time – any variation in capitalization or spacing in the name will cause the program to 
               recognize this as a different nest, e.g. nest1, nest 1, and Nest1 would be considered three separate 
               nests. Status should be an integer from 1-8. Missing values are not allowed in any of these fields and 
               must be filled in before analysis."),
             h3("Nest Status Definitions and Guidelines"),
             h4("1 = Active"),
             p("Active indicates that eggs are present and the nest is being attended by an adult, or if no adult is 
               present that there is additional evidence that the nest has not been abandoned (e.g., fresh plover 
               tracks)."),
             h4("2 = Depredated"),
             p("Nest has been depredated, determined via the following evidence criteria: eggs broken, or eggs 
               missing plus presence of predator tracks of scat/pellets at the nest; of a lack of direct evidence
               other than eggs missing but other factors such as flooding or abandonment were ruled out."),
             h4("3 = Weather-related or Tidal Flooding"),
             p("Nest is completely or partially washed out following heavy rain or high tides, and must be determined 
               by a person already familiar with the nest’s location; or nest completely or partially sanded in 
               following a storm or unusually strong winds. Note that nests plovers may excavate and continue to 
               incubate eggs that have been sanded in, so we recommend continued monitoring before making this 
               determination."),
             h4("4 = Abandoned"),
             p("Eggs remain in the nest with no plover tracks, nest is lightly sanded over (as a result of adult 
               absence rather than due to a storm or unusually strong winds), or if adult mortality is known to have 
               occurred, or if adults were missing during at least the last two checks."),
             h4("5 = Unknown Cause of Failure"),
             p("Choose this option if eggs are missing and nest is unlikely to have hatched based on estimated nest 
               age, but cause cannot be assigned based on the criteria listed for predation, abandonment, and 
               flooding/weather."),
             h4("6 = Hatched"),
             p("Designate the nest as hatched if the eggs are missing, it is near or after the expected hatch date, 
               and either a newly-hatched brood is seen nearby or there is no evidence of nest failure."),
             h4("7 = Unknown Status"),
             p("Use this code if uncertain whether a nest has failed or hatched."),
             h4("8 = Other Cause of Failure"),
             p("Use this code to indicate known sources of nest failure that do not fit in the remaining categories, e.g.
               eggs trampled by pedestrians."),
             h3("C. Uploading the Data"),
             p("Click on the", span("Upload Data", style="color:blue"),"tab at the top of the screen to navigate to the Upload section. 
               At the top of the sidebar is a section titled", span(strong("Upload Nest Data File")) ,"; press the button 
               labeled", span(code("Browse...", style="color:black")),"to select and upload a file from your computer. Once the file has been selected, the tool 
               uploads your data, displays it as a table, and automatically calculates some summary statistics.  
               The data table should be displayed to the right (on some browsers it may show up at the bottom of
               the page), and under", span(strong("Summary of Data Entered")) ,"you will see a count of how many unique nests are 
               included in the dataset and how many of each nest fate has been entered."),
             img(src="enteredData.png"),
             h3("D. Proofing and Editing Your Data"),
             p("After you have uploaded data, you can edit existing entries or add new entries if needed. 
               To insert a new row of data, right-click anywhere in the table and select 
               either", span(code("Insert row above", style="color:black")),"or", span(code("Insert row below", style="color:black")),"; 
               a row can be deleted by selecting ", span(code("Remove row", style="color:black")),". The Nest.ID 
               column cells contain a drop-down menu (click on the gray arrow in the right side of the cell) that 
               includes all previously-entered nest names, or you can click on the cell and manually enter a new name. 
               To modify or enter a new date, click on the gray arrow to pull up a calendar and select the desired 
               date."),
             p("The Status column contains a dropdown menu with integers from 1 to 8, and the Exclosed column 
               contains a drop down menu with Y and N options. The Interval column is automatically filled in 
               before analysis and cannot be edited by the user."),
             p("You can sort the data by any column by first right-clicking once on the column name, which will 
               simultaneously highlight the column in blue and sort the table by the column in ascending order. 
               Click a second time to sort in descending order."),
             h3("E. Display Cleaned Data"),
             p("The tool will initially display your dataset exactly as it is arranged in the file you uploaded. 
               However, before the tool enters the nest fate analysis phase, it prepares the data for analysis by 
               sorting the dataset by nest ID and date, calculating nest check interval lengths, removing any extra 
               columns, and removing any post-hatch data that might be included. If you wish to see how your data look
               after the cleaning process, press the button labeled", span(code("Display Cleaned Data", style="color:black")),". This will arrange the table
               by nest and date, fill in the interval column, and remove post-hatch rows. This step is not required
               before moving on, but it is recommended as an additional error-proofing opportunity. The first entry 
               for each nest should have a blank Interval entry (because the nest had not been previously checked), 
               and all other intervals should be strictly positive."),
             p("If you wish to save a copy of your cleaned data, press the button 
               labeled ", span(code("Download Cleaned Data", style="color:black")),", which will open your cleaned data as a .csv file."),
             h3("Troubleshooting and Tips"),
             p("Ensure that the number of nests entered under", span(strong("Summary of Data Entered")),"matches 
              your expectations – a nest count greater than you believe you have entered is indicative of misspelled 
               nest names in your data."),
             p("•	Arrange the data and check the Interval column – if you see an unusually long interval 
               (e.g., > 30 days), double-check the dates of that and the preceding row."),
        #######################
        #nest fate analysis section of instructions
        h2("Nest Fate Analysis"),
        p("Once you have uploaded and checked your data, this section analyzes your nest fate data in order 
               to estimate abandonment, predation, and flooding/sanding rates of unexclosed and exclosed nests.
               Navigate to the tab labeled", span("Decision Support", style="color:blue"), ". To begin the analysis,
               press the button labeled", span(code("Analyze Nest Fate Data", style="color:black")), ". If you 
               have not yet uploaded any data, a red warning
               will appear asking you to go back and upload data. A status bar will appear in the bottom right corner
               of the screen saying", code("Calculating, this may take a few minutes", style="color:black"), ". NOTE: 
               unlike in later modules, the blue status bar does not move with the progression of the model. 
               Model run time will vary but in early testing it has rarely exceeded one minute.  
               Once the model has finished running, the status bar will disappear and 
               the", span(strong("Fate Probabilities")), "table will be populated with your estimates, which will be displayed as
               probabilities with the standard error in parentheses."),
        img(src="fateProbs.png"),
        h3("Interpreting Your Results"),
        p("It is good practice to examine your nest fate analysis results to make sure the estimates are roughly 
          in line with your knowledge of your site and to check the magnitude of the standard errors. If your
          dataset does not contain enough information (for example, few nests and no abandonments), 
          standard errors may reach or exceed 50% of the estimate values, and estimates may seem wildly 
          off-base. If this happens, it is best not to proceed to
          the", span(strong("Decision Support Population Modeling")), "section, because those models use the results
          of your nest fate analysis; instead, skip to", span("Scenario Modeling", style="color:blue"), ". While we have 
          no firm cutoff guidelines, below is an example that shows unreasonable estimates 
          and large standard errors; this is an extreme case that would indicate 
          skipping to", span("Scenario Modeling", style="color:blue"), "."),
        img(src="nestfateExtreme.png"),
      #############
      #Decision Support Instructions Section
      h2("Decision Support Population Modeling"),
      p("IMPORTANT: The models in this section use values from your", span(strong("Nest Fate Analysis")), ", thus 
        you must have completed that section first before using this one."),
      p("The", span(strong("Decision Support Population Modeling")), "section in the", span("Decision Support", style="color:blue"), "tab provides
       the analysis summaries that you can use to make a decision about exclosure use. The analyses in this module take the
       estimates of predation risk, abandonment risk, and exclosure effects that were estimated for your site
       in the", span(strong("Nest Fate Analysis")), "section and plug them into a stochastic population projection model in order 
       to predict population growth rate at your site with and without the use of exclosures."),
      h3("3D Guideline Plot"),
      p("The first thing you see on this page is a 3-dimensional surface plot. The data displayed here are the result 
        of simulations used to calculate the expected gain (which can be positive or negative) in population growth 
        rate at a hypothetical site as a function of that site’s exclosure-related abandonment risk and predation
        risk for unexclosed nests. Positive values are shown in green and represent an increase in population growth 
        with exclosures compared to not using exclosures (note: population growth rate might be negative both with
        and without exclosures at a particular site, and yet the", span(em("difference")), "in growth rate between using exclosures
        and not using them, shown in the 3-D plot, could still be positive). Yellow, orange, and red values depict 
        increasingly negative effects of exclosure use on population growth rate. The solid black line shows where 
        the difference is exactly zero, i.e. population growth rate does not depend on whether or not exclosures 
        are used. If you have already run the nest fate analysis, a point representing your site’s location on 
        this surface will show up, surrounded by a circle that represents the degree of uncertainty in your site’s
        location. This plot is not intended to be used alone to make the decision; rather it is intended as a 
        supporting visual aid."),
      img(src="threeDplot.png"),
      h3("Population Growth Rate"),
      p("This section has a button for the user to click, which retrieves the information from the nest fate 
        analysis and performs the population growth rate calculations. Once the calculations are complete 
        (it may take several minutes), a plot appears underneath the button that displays the mean predicted
        population growth rate with and without exclosures, bounded by the 95% prediction intervals. The horizontal
        bar in the graph indicates the location of growth rate = 1, in other words a stationary population; values 
        that appear above this line indicate a growing population, while values below this line indicate a 
        declining population. "),
      img(src="lambdaPlot.png"),
      h3("Making a Decision"),
      p("Below the plot is a table that summarizes the outcomes of the population growth predictions; 
        it displays the probabilities of rapid growth (growth > 5% per year), any growth, any decline, and rapid 
        decline (decline > 5% per year) as a function of exclosure use. The third column summarizes how exclosure
        use affects the probabilities of each outcome. THESE ARE THE MAIN DECISION CRITERIA. In general, if 
        exclosure use results in increased growth probability and decreased decline probability, the decision 
        should be to use exclosures. However, the user will need to consider their own logistics and values when 
        using these probabilities for decision-making – for instance, if probability of growth with exclosures 
        is only 10% greater than not using exclosures, it might not be worth the effort for some sites with 
        difficult access issues."),
      p("It  is also important to recognize that the population projection model underlying these 
        outcomes does not account for site carrying capacity or dispersal rates, in large part because 
        these factors are unknown. If the site population is predicted to grow as a result of management,
        extra birds might disperse to other parts of the region if the modeled site is already at carrying 
        capacity – thus site-level abundance may stay the same even if the site’s population is growing.
        This model therefore predicts the capacity for growth in the absence of density dependence.  Future 
        versions of this model might incorporate site-specific carrying capacity, or region-level effects of 
        a site-specific management decision."),
      
      #Abandonment Tolerance section
      h2("Abandonment Tolerance"),
      p("IMPORTANT: The models in this section use values from 
        your", span(strong("Nest Fate Analysis")), "and", span(strong("Decision Support Population Modeling")), ", thus 
        you have to have completed those sections first before using this one."),
      p("If you have chosen to use exclosures, the", span(strong("Abandonment Tolerance")), "section provides 
        guidance on how many total abandonments your site can handle before you need to reassess or reverse your 
        decision, given that conditions might change unexpectedly if, say, a predator that targets exclosures
        shows up mid-season. Abandonment tolerance is estimated by simulating an increasing baseline level of 
        abandonment risk  while keeping predation risk constant (with mortality risk set at the expert opinion
        level of 70% per observed abandonment, with or without exclosures), and compares the resulting probability 
        of population decline to the probability of decline if exclosures were not used. "),
      p("An important assumption of this model is that whatever is causing exclosure-related abandonments 
        is not also targeting unexclosed nests. Such a scenario is not currently accommodated in this model 
        but may be developed in future versions. "),
      p("To proceed, first select the number of pairs present at your site
        under", span(strong("Starting # Pairs")), ". Then press
        the button labeled", span(code("Calculate Threshold", style="color:black")), ". A 
        status bar will appear in the bottom left of
        the screen with text stating", span(code("Calculating Thresholds", style="color:black")), ". This may take a few minutes, 
        after which a plot will appear underneath the", span(strong("Threshold Plot")), "heading."),
      img(src="thresholdPlot.png"),
      h3("Interpretation"),
      p("The thresholds plot will have a curve with cross-hatching underneath, which represents the probability
        of population decline as a function of observed exclosure-related abandonments. The dashed horizontal 
        line represents the reference condition of no exclosures. Where the two lines intersect is the point at
        which probability of decline with and without exclosures is identical; this number will also appear in the
        text below the plot as", span(strong("Reassess or pull exclosures after X observed nest abandonments")), "."),
      p("If you make this assessment at the start of the breeding season, we recommend that you analyze new data 
        either 1) after June 1, or 2) after observing X nest abandonments according to this analysis, whichever
        comes first. If you have made this assessment after June 1, then analyze new data (if possible) or consider
        pulling exclosures after observing X  nest  abandonments."),
      
      #Scenario Modeling
      h2("Scenario Modeling"),
      p("The", span("Scenario Modeling", style="color:blue"), "tab provides users a way to explore hypothetical scenarios. 
        This is useful for users in charge of small sites who thus lack sufficient data to run the previous analyses,
        or for anybody to explore effects of hypothetical scenarios, for example:"),
      p("1. Effects of alternative management actions – if you believe a change in predator control practices might 
        reduce the predation risk of unexclosed nests, how will that affect your decision?"),
      p("2. Effects of increased adult mortality risk – if you have a raptor known to target adult plovers at 
        exclosures, you can explore a worst case scenario in which abandonment is 100% indicative of adult mortality."),
      p("There are three sliders that allow users to toggle values for predation risk without exclosures, 
        abandonment risk with exclosures, and probability of an adult mortality given nest abandonment. 
        The default values are set to range-wide average values for predation and abandonment based on our 2015
        and 2016 analyses, and the default mortality risk is based on the average of four expert opinion estimates. 
        To change the default values, click on the gray circle on the slider and move it to your desired value. 
        Because they are probabilities, the sum of predation and abandonment probabilities must be less than 
        100 – if you choose values that equal or exceed 100, a warning in red will appear underneath the sliders 
        stating", span("Warning: Predation Risk + Abandonment Risk must = less than 100%", style="color:red"), ". 
        Once you have selected your values, click the
        button labelled", span(code("Press to Calculate", style="color:black")),". If 
        at any time you want to reset the values to their default settings, 
        click the", span(code("Press to Reset Values", style="color:black")),"button."),
      p("The model will run for up to 30 seconds, and once it is completed the results will appear on the screen. 
        To the right of the sliders a graph will appear showing predicted growth rates with and without exclosures.
        Below the sliders, the table will be populated with probabilities of rapid growth (>5% annually), any growth,
        any decline, and rapid decline (>5% annually), along with the relative effects of exclosure use on these
        probabilities."),
      img(src="scenModelExample.png"),
      p("For discussion on how to interpret these values and come to a decision, please see the section
        above titled", span(strong("Making a Decision")),".The", span("Scenario Modeling", style="color:blue"), "tab 
        includes an", span(strong("Abandonment Tolerance")),"section to calculate how many 
        nest abandonments a site can sustain before the benefits of exclosures do not exceed the risks,
        based on the values for predation, abandonment, and mortality rated that the user has chosen; 
        for details on use and interpretation of this section, please refer to the section above
        called ", span(strong("Abandonment Tolerance")),"."),
      ##############
      
      #Generate report
      h2("Generate Report"),
      p("Both the", span("Decision Support", style="color:blue"), "and", span("Scenario Modeling", style="color:blue"), "tabs 
        have buttons at the bottom of the screen that allow users to download a report summarizing their analyses.
        Each tab has two clickable
        buttons:", span(code("Generate HTML Report", style="color:black")),"and", span(code("Download Report in MS Word", style="color:black")),". Click 
        on", span(code("Generate HTML Report", style="color:black")),"to open a web page with the results, which can 
        be sent to a printer or saved as an HTML document offline for later use. 
        Click on", span(code("Download Report in MS Word", style="color:black")),"to download the report as a word file that
        can be opened in Microsoft Word. Reports from the", span("Decision Support", style="color:blue"), "tab contain
        a summary of the data entered, nest fate probabilities, population modeling plot and trajectory probabilities,
        starting # of pairs, and the abandonment tolerance plot. Sections not run are left blank in the report or
        populated with NAs. Reports for from the", span("Scenario Modeling", style="color:blue"), "tab include the
        values of predation, abandonment, and mortality risk chosen, population modeling plot and trajectory 
        probabilities, starting # of pairs, and the abandonment tolerance plot."),
      br(),
      br()
    )#intructions panel
    
               )#tabsetPanel
             )) #shinyUI
