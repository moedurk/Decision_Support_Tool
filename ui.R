library(rhandsontable) #needs to be loaded from ui file, unlike other packages

#ui####

shinyUI(fluidPage(
  titlePanel("PiperEx: Piping Plover Decision Support Tool for Exclosure Use"),
  tabsetPanel(
    #Introduction####
    tabPanel("Introduction",
             br(),
             sidebarPanel(
               h3 (strong("Purpose")),
               p("To provide site biologists with guidance for deciding whether or not to use
                 nest exclosures for Piping Plover management", style="font-size:14pt"),
               br(),
               h3(strong("Version 2.0, March 2018")),
               p("Developed by Abigail Darrah and Jonathan Cohen, State University of New York College of
                 Environmental Science and Forestry"),
               br(),
               h3(strong("Getting Started")),
               helpText("Detailed instructions are available to read in the", strong("Instructions"),
                        "tab or to download below:", style="font-size:12pt"),
               downloadButton("instructions","Download Instructions"),
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
             h1(strong("Decision Support Analysis"), style="color:#000066"),
             fluidRow(
               column(6,
             wellPanel(
               fluidRow(
                 h3(strong("Enter # Pairs at Site"), style = "color:#000066; padding: 0px 0px 0px 10px"),
                 column(3,
                        numericInput("Pairs", label="", value=20, min=1, step=1))
               ),
               h3("Press button below after uploading nest data file", style="color:#000066"),
               actionButton("analyze.go", "Analyze Nest Fate Data"),
               br(),
               br(),
               helpText("Analysis may take a few minutes. Results will be displayed below once the model has finished running.
                        Probabilities will be displayed as percentages, rounded to the nearest 1 percent,
                       with standard error in parentheses", style="font-size:12pt"),
               h3(textOutput("SmallSampleWarn"), style="color:red")
               ) #wellPanel
              ) #column
             ), #fluidRow
             fluidRow(
               h2(strong("Nest Fate Probabilities, mean (standard error)",style="color:#000066; padding:0px 0px 0px 10px")),
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
                        h2(strong("Population Growth Rate", style="color:#000066")),
                        helpText("The violin plot below shows the predicted population growth rates (lambda, λ)
                         with and without nest exclosure use based on the nest data that you uploaded. The black
                         diamonds represent the mean growth rate for each scenario, while the horizontal width
                         of the gray shaded area represents the relative likelihood of a given growth rate in
                         each scenario. The solid black line represents a population growth rate of 1.0,
                         which indicates a stable population. Values above 1.0 indicate an increasing population,
                         and values below 1.0 represent a declining population.

                         For example, a violin that is short and wide at a growth rate of 0.8 would mean a high likelihood of
                         observing a growth rate of 0.8 under the scenario. A violin that is tall and thin, stretching from
                         0.5-1.0 would indicate a wider range of potential   growth rates with less certainty around one value.
                          Uncertainty may be decreased in some cases by providing the model with a larger sample of nests.", style="font-size:12pt"
                         ),
                        br(),
                        plotOutput("lambdaplot")
                        )
                      )
             ),
             fluidRow(column(4,
                             h2(strong("Trajectory Probabilities", style="color:#000066; padding:0px 0px 0px 10px")),
                             helpText(These percentages are the likelihood of each potential population outcome with and without exclosures.
                             Rapid growth= lambda >1.05 (5% growth per year or more), Any growth= lambda >1.0, Any decline= lambda< 1.0 , Rapid decline= lambda<0.95",
                             style="font-size:12pt"

                             )

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
         h1(strong("Abandonment and Predation Tolerance"), style="color:#000066"),
         fluidRow(
           column(8,
                  wellPanel(
                    helpText("This module provides guidelines for when to reassess your decision to use or not use exclosures.
                     The dotted line on each graph is the probability of a population decline at your site based on current conditions.
                     The solid lines are the probability of a population decline under the scenario with increasing numbers of nest abandonments
                     or depredations.

                      The first plot indicates how many nest abandonments you can tolerate if you decide to use exclosures at the start of the season.
                      If you observe the indicated number of exclosed nest abandonments, the likelihood of a population decline becomes greater than if
                      you had taken no action (where the solid line exceeds the value of the dashed line). At this point, we recommend you reassess the
                      situation with updated data or pull exclosures.

                      The second plot indicates how many nest predations you can tolerate if you decide not to use exclosures.If you observe the indicated
                      number of nest predations, the likelihood of a population decline is higher than if you did use exclosures,  due to high incidence
                      of nest depredations. Under this scenario, the benefits of exclosure use likely outweigh the risks andwe recommend reassessment or using exclosures.
                      ", style="font-size:12pt")
                        ))),

         fluidRow(
           h3(strong("If you plan use exclosures..."), style = "color:#000066; padding: 0px 0px 0px 10px"),
           column(3,
                  actionButton("threshold.data", "Calculate Threshold")
           ) #putting button in a column puts some space between button and left edge of screen
         ), #fluidrow
         fluidRow(
           column(8,
                  br(),
                  h3(strong("Abandonment Threshold Plot"), style = "color:#000066"),
                  plotOutput("thresh.abans")
           ) #column
         ), #fluidrow
         fluidRow(
           wellPanel(
             h3(textOutput("reassess"))
           )
         ),#fluidRow
         br(),
         fluidRow(
           h3(strong("If you do NOT plan to use exclosures..."), style = "color:#000066; padding: 0px 0px 0px 10px"),
           column(3,
                  actionButton("threshold.pred.data", "Calculate Threshold")
           ) #putting button in a column puts some space between button and left edge of screen
         ), #fluidrow
         fluidRow(
           column(8,
                  br(),
                  h3(strong("Predation Threshold Plot"), style = "color:#000066"),
                  plotOutput("thresh.preds")
           ) #column
         ), #fluidrow
         fluidRow(
           wellPanel(
             h3(textOutput("reassess.pred"))
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
                   h1(strong("Scenario Modeling"), style = "color:#000066"),
                   helpText("This module allows you to explore the effects of hypothetical scenarios based on
                   different values of predation rates, abandonment rates, and exclosure-related adult mortality risk
                    that you set. You also have the option to import the parameter values from the decision support
                     modeling you just completed, by selecting the checkbox under the “Choose Parameters” heading.", style="font-size:12pt")
                   ) #wellPanel
                 ) #column
               ), #fluidRow
        fluidRow(
          column(3,
          wellPanel(
          h2(strong("Choose Values"), style = "color:#000066"),
          fluidRow(
            h3(strong("Enter Number of Pairs"), style = "color:#000066; padding: 0px 0px 0px 10px"),
            column(5,
                   numericInput("ScenPairs", "Starting # Pairs", value=20, min=1, step=1))
          ),
          h3(strong("Choose Parameters"), style = "color:#000066;
             padding: 0px 0px 0px 10px"),
          checkboxInput("import", "Import from Decision Support Analysis"),
          br(),
          actionButton("reset_input", "Reset to Default Values"),
          br(),
          br(),
          uiOutput("resettableScenarioValues"),
             actionButton("scenario","Press to Calculate")
             ) #wellPanel
          ),#column
          column(5,
                 wellPanel(
                 h2(strong("Population Growth Rate"), style = "color:#000066; padding: 0px 0px 0px 10px"),
                 helpText("The plot below shows the predicted population growth rates with and without exclosure
                  use based on the parameters you choose. The black diamonds represent the mean growth rate for each
                   scenario, while the width of the gray shaded area represents the relative likelihood of a given growth
                    rate in each scenario."),
                 plotOutput("scenLambdaPlot")
                 )#wellPanel
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
        #Abandonment and Predation Tolerance for Scenario####
        h2(strong("If you plan to use exclosures"),style="color:#000066"),

        fluidRow(
          h3(strong("Press to Calculate Threshold"), style = "color:#000066; padding: 0px 0px 0px 10px"),
          column(3,
                 actionButton("Scen.threshold.data", "Calculate Threshold")
          ) #putting button in a column puts some space between button and left edge of screen
        ),

            br(),

        fluidRow(
          column(8,
                 br(),
                 h3(strong("Abandonment Threshold Plot"), style = "color:#000066"),
                 plotOutput("scen.thresh.abans")
          ) #column
        ), #fluidrow
        fluidRow(
          wellPanel(
            h3(textOutput("SCENreassess"))
          )
        ),#fluidRow
        br(),
        fluidRow(
          h2(strong("If you do NOT plan to use exclosures..."), style = "color:#000066; padding: 0px 0px 0px 10px"),
          column(3,
                 actionButton("scen.threshold.pred.data", "Calculate Threshold")
          ) #putting button in a column puts some space between button and left edge of screen
        ), #fluidrow
        fluidRow(
          column(8,
                 br(),
                 h3(strong("Predation Threshold Plot"), style = "color:#000066"),
                 plotOutput("scen.thresh.preds")
          ) #column
        ), #fluidrow
        fluidRow(
          wellPanel(
            h3(textOutput("scen.reassess.pred"))
          )#wellpanel
        ),#fluidrow
        downloadButton("ScenReport", "Generate HTML Report"),
        downloadButton("ScenReportWord", "Download Report in MS Word"),
        br(),
        br()
    ), #scenario panel
 #Instructions####
    tabPanel("Instructions",
             h2("Introduction"),
             p("Nest exclosures are frequently used to increase hatch success of threatened and
                 endangered populations of Piping Plover (",span(em("Charadrius melodus")),").
                 However, exclosure use can result in adult mortality if adult predators key into
                 the presence of exclosures, or if adults are impeded from escaping predators.
                 Adult mortality is not always detected in the absence of a carcass, other signs
                 of mortality, or a nest camera. If one adult of a pair is killed, the remaining
                 parent sometimes abandons the nest as a result. In this scenario, it is not possible
                 to determine whether one or both adult plovers have died or have truly abandoned the
                 nest and are alive elsewhere, unless both adults are banded and are resighted quickly.
                 As a result, adult mortalities can be misclassified as true nest abandonments.
                 Exclosure use thus increases nest success by reducing nest predation but can
                 decrease adult survival rates, with unknown consequences for population growth rate.
                 Modeling efforts based on 1312 nests from the U.S. Atlantic Coast revealed unpredictable geographic and temporal variation in nest abandonment (and thus mortality) probability with exclosure use, thus the decision to use exclosures needs to be made on a site-by-site and year-by-year basis. Depending on the site and year, the risk of adult mortality from exclosure use can be outweighed by the increase in nest survival, in which case their use is prudent. In other cases, the adult mortality effects outweigh the benefit to nest survival, and exclosure use can have the opposite of the intended impact on populations. "),
             h3("Purpose"),
             p("To help site managers make early- and mid-season decisions about exclosure use for Piping Plover conservation."),
             h3("How It Works"),
             p("This tool accepts nest fate data from the user, and analyzes the data to provide site-specific
               estimates of exclosure effects and nest abandonment and predation probabilities. These values are then
               used to predict season-long productivity and breeding adult survival rates, which are combined with
               published or expert opinion values of other demographic rates to estimate population growth rate at the
               user's site with and without exclosures. The results can support a decision to use exclosures or not,
               based on whether exclosures increase or decrease population growth rate compared to not using exclosures."),
             h3("Model Assumptions and Limitations"),
             p("The models underlying this tool were parameterized using data from the Atlantic Coast population
               of Piping Plovers, and thus might not be strictly applicable to Great Lakes or Great Plains populations.
               The model assumes that all nest fates have been identified correctly; uncertain fates are best recorded
               as unknown rather than given a best guess, which could bias your results and affect the decision.
               The population growth rates are calculated using average survival and fledgling estimates from the
               literature or expert opinion; if your site or region experiences different vital rates, this may affect
               the accuracy of the population growth estimates but will not affect whether using exclosures is better
               than not using exclosures. The scenario modeling tab can be used to project population growth rates using
               chick survival rates that differ from baseline values from the literature. "),
             h2("Overview of Steps"),
             p("Below is a quick reference outlining the steps to take when using this tool. Detailed instructions for
               each step are provided in the remainder of this document."),
             h3("1. Decide when and where to start"),
             p("We recommend using this tool twice during the breeding season: once at the beginning to make an
               initial management decision based on the previous year's data, and again after June 1 or after you
               have accumulated new data for at least 10 complete nests (complete meaning the nests have hatched or
               failed). If you have data, start in the", span("Upload Data", style="color:blue"), "tab to load your data into the tool and move on to
               the next Step. If you do not have data, and/or you wish to explore hypothetical situations, you start
               in the", span("Scenario Modeling", style="color:blue"),"tab."),
             h3("2. Upload Data"),
             p("Using the features in the", span("Upload Data", style="color:blue"), "tab, load and double-check your
               nest fate data."),
             h3("3. Decision Support Analysis"),
             p("The first step in the", span("Decision Support", style="color:blue"),"tab analyzes your data to provide estimates of
               nest hatch, abandonment, predation, and tidal- or weather-caused failure probabilities for exclosed
               and unexclosed nests.It then uses the estimates from your nest fate analysis to provide predicted population growth rates
               for your site with and without exclosures."),
             h3("4. Abandonment and Predation Tolerance"),
             p("The Abandonment and Predation Tolerance module in the", span("Decision Support", style="color:blue"),"tab is an
               optional set of models that estimate: 1) how many exclosure-related apparent abandonments (which may actually be undetected adult mortalities)
               you can tolerate before reassessing or removing your exclosures, and 2) how many predations of unexclosed nests you can tolerate before
               reassessing or putting up exclosures."),
             h3("5. Scenario Modeling"),
             p("The", span("Scenario Modeling", style="color:blue"),"section allows users to choose different
               values of predation, abandonment, and adult mortality risk, and to see the predicted outcomes of
               exclosure use in these hypothetical scenarios.  It is useful when there are limited or no pre-existing
               data, but it can also be used in conjunction with a user's existing data by importing the results of the
               Decision Support Analysis"),
             h3("6. Generate Report"),
             p("In both the", span("Decision Support", style="color:blue"),"and",
               span("Scenario Modeling", style="color:blue"),"sections include buttons that will generate either
               HTML or Word document reports summarizing the user's analyses. "),
             h2("Getting your data into the tool"),
             p("To begin your analysis, navigate to the", span("Upload Data", style="color:blue"),"tab of the tool"),
             img(src = "uploadOverview.png"),


             h3("Sample Size Requirements"),
             p("This tool treats all data supplied as a single site. Before gathering your data for analysis,
               you will need to decide what constitutes a site. This question is particularly relevant for managers
               of either large areas that could be divided into smaller sites, or managers of multiple small sites
               within a region. In general, a site is an area subject to a single management plan and experiencing
               largely similar environmental and anthropogenic conditions. If you wish to treat management units within a larger area as
               separate sites, then data for each unit needs to be run separately and will need to meet the minimum
               sample size criteria (discussed below). It will thus be preferable in most cases to combine adjacent
               management units to provide better estimates to inform the model, but if there are enough nests in each
               unit to be run separately, it may be worthwhile to compare the resulting decision for one large site
               versus treating it as several smaller sites."),

             p("The tool will run regardless of sample size, but we recommend a minimum of 10 nests from a single season be uploaded for
               getting reliable site- and year-specific recommendations. This sample size cutoff is based on
               preliminary simulations to calculate the probability of making the correct decision as a function of
               sample size. With 5 nests, the probability of making the correct decision is 55% (i.e. you might as
               well flip a coin). With 10 nests the probability is 80%, and the probability continues to increase as
               sample size gets larger. To obtain the most reliable estimates, users should upload all available nest data (i.e. highest
               number of nests available) from a single season."),

             p("If you do not have at least 10 complete nests (i.e. nests that have hatched or
               failed) from either the previous year or for the current year, we recommend
                choosing one of the following options: 1) combine data from multiple years for your site; 2)
                combine your data with nearby similar sites (see above criteria for combining sites); or 3)skipping the data
               upload and analysis and moving straight to", span("Scenario Modeling", style="color:blue"),".
               If you choose option 1 (combine multiple years of data), you will get an average site-specific
               prediction of the risks/benefits of exclosures, and the resulting decision will thus represent the
               preferred management action in an average year - additional scenario modeling will be necessary if
               you believe the current year is unusual in terms of either nest predation or abandonment rates. For
               choosing option 2 (combine sites), consider whether the sites are likely to experience similar factors
               that influence nest predation and adult mortality risk."),

             p("A final note about the data - if there are no exclosed nests in your dataset, then the exclosure effects
               for your site will be based on the Atlantic coast-wide average. Therefore, if you typically do not use
               exclosures but want to predict the effects of using them at your site, you may want to consider
               adding in older data from your site that includes exclosed nests if available; otherwise, keep in
               mind that the resulting predictions are based on average exclosure effects in combination with your
               site-specific predation and abandonment estimates. If the likelihood of an exclosure-related adult mortality
                or abandonment at your site differs significantly from the average (perhaps because of an abundance of certain
                predator types), the resulting predictions will not predict the consequences of exclosing at your site very accurately."),


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
               same way each time - any variation in capitalization or spacing in the name will cause the program to
               recognize this as a different nest, e.g. nest1, nest 1, and Nest1 would be considered three separate
               nests. Status should be an integer from 1-8. Missing values are not allowed in any of these fields and
               must be filled in before analysis."),

           h3("Nest Status Definitions and Guidelines"),
             h4("1 = Active"),
             p("Active indicates that eggs are present and the nest is being attended by an adult, or if no adult is
               present that there is additional evidence that the nest has not been abandoned (e.g., fresh plover
               tracks). This status includes both the laying and incubation periods"),
             h4("2 = Depredated"),
             p("Nest has been depredated, determined via one of the following evidence criteria: 1) eggs broken
             2)eggs missing plus presence of predator tracks of scat/pellets at the nest; 3) eggs missing, but a
             known expected hatch date obtained from floating or from location of nest during laying rules out
             hatching PLUS a lack of evidence of flooding."),
             h4("3 = Weather-related or Tidal Flooding"),
             p("Nest is completely or partially washed out following heavy rain or high tides, and must be determined
               by a person already familiar with the nest's location; or nest completely or partially sanded in
               following a storm or unusually strong winds with no evidence of adult presence. Note that nests plovers may excavate and continue to
               incubate eggs that have been sanded in, so we recommend continued monitoring before making this
               determination."),
             h4("4 = Abandoned"),
             p("Eggs remain in the nest with no plover tracks, nest is lightly sanded over (as a result of adult
               absence rather than due to a storm or unusually strong winds), or if adult mortality is known to have
               occurred, or if adults were missing during at least the last two checks. To confirm abandonment,
               it can be useful to clear a small area of sand at an entry or exit point and re-check it for tracks
               to confirm if cryptic adults may be visiting."),
             h4("5 = Unknown Cause of Failure"),
             p("Choose this option if eggs are missing and nest is unlikely to have hatched based on estimated nest
               age, but cause cannot be assigned based on the criteria listed for predation, abandonment, and
               flooding/weather."),
             h4("6 = Hatched"),
             p("Designate the nest as hatched if the eggs are missing and it is near or after the expected hatch date,
               and either a newly-hatched brood is seen nearby or there is no evidence of nest failure."),
             h4("7 = Unknown Status"),
             p("Use this code if uncertain whether a nest has failed or hatched."),
             h4("8 = Other Cause of Failure"),
             p("Use this code to indicate known sources of nest failure that do not fit in the remaining categories, including
               eggs crushed by pedestrians, animals, vehicles, or nests which don't hatch in 28 days due to infertility or in-egg death."),


            h3("Uploading Your Data"),
             p("Click on the", span("Upload Data", style="color:blue"),"tab at the top of the screen to navigate to the Upload section.
               At the top of the sidebar is a section titled", span(strong("Upload Nest Data File")) ,"; press the button
               labeled", span(code("Browse...", style="color:black")),"to select and upload a file from your computer. Once the file has been selected, the tool
               uploads your data, displays it as a table, and automatically calculates some summary statistics.
               The data table should be displayed to the right (on some browsers it may show up at the bottom of
               the page), and under", span(strong("Summary of Data Entered")) ,"you will see a count of how many unique nests are
               included in the dataset and how many of each nest fate has been entered. You may only assign one final fate per nest.
               If eggs in a nest are lost to different sources- for example if a crow takes 2 eggs, and then 10 days later the other 2
               are washed out- it is up to the user to determine which fate they wish to assign. Remember, PiperEx is trying to calculated
               the daily probability that a nest is lost to a particular source.  As a means of error-proofing users should verify in the Summary
               of Data Entered display that the number of nests lost to each fate sums to the total number of unique nests in your dataset. If
               PiperEx is detecting more nests than are in your dataset, this is likely because you entered multiple fates for the same nest ID.),
             img(src="enteredData.png"),


          h3("Proofing and Editing Your Data"),
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

            h3("Display Cleaned Data"),
             p("This step is optional but recommended as a final error-proofing opportunity.
               The tool will initially display your dataset exactly as it is arranged in the file you uploaded.
               However, before the tool enters the nest fate analysis phase, it prepares the data for analysis by
               sorting the dataset by nest ID and date, calculating nest check interval lengths, removing any extra
               columns, and removing any post-hatch data that might be included. If you wish to see how your data look
               after the cleaning process, press the button labeled", span(code("Display Cleaned Data", style="color:black")),". This will arrange the table
               by nest and date, fill in the interval column, and remove post-hatch rows.The first entry
               for each nest should have a blank Interval entry (because the nest had not been previously checked),
               and all other intervals should be positive integers."),
             p("If you wish to save a copy of your cleaned data, press the button
               labeled ", span(code("Download Cleaned Data", style="color:black")),", which will open your cleaned data as a .csv file."),
             h3("Troubleshooting and Tips"),
             p("Ensure that the number of nests entered under", span(strong("Summary of Data Entered")),"matches
              your expectations - a nest count greater than you believe you have entered is indicative of misspelled
               nest names in your data."),
             p("Ensure that the number of nests for the different nest fates sums to the total number of nests.
                A sum of the nest fate categories exceeding the total number of nests is indicative of errors in the
                nest status column, usually that multiple fates have been entered for the same nest ID."),
             p("Arrange the data and check the Interval column - if you see an unusually long interval
               (e.g., > 30 days), double-check the dates of that and the preceding row."),
        #######################
        #nest fate analysis section of instructions
        h2("Decision Support Analysis"),
        p("Once you have uploaded and checked your data, this section analyzes your nest fate data in order
               to estimate abandonment, predation, and flooding/sanding rates of unexclosed and exclosed nests.
               Navigate to the tab labeled", span("Decision Support", style="color:blue"), ". To begin the analysis,
               enter the number of Piping Plover pairs present at your site and then press
                the button labeled", span(code("Analyze Nest Fate Data", style="color:black")), ". If you
               have not yet uploaded any data, a red warning
               will appear asking you to go back and upload data. Two status bars will appear in the bottom right corner
               of the screen saying", code("Calculating, this may take a few minutes", style="color:black"),"
               and", code("Calculating Projections...", style="color:black"),". NOTE:
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
          and large standard errors; this is an extreme case where the user should skip to ", span("Scenario Modeling", style="color:blue"), "."),
        img(src="nestfateExtreme.png"),
      #############
      #Decision Support Instructions Section
      h2("Decision Support Population Modeling"),
      p("The", span(strong("Decision Support Population Modeling")), "section in the", span("Decision Support", style="color:blue"), "tab provides
       the analysis summaries that you can use to make a decision about exclosure use. The analyses in this module take the
       estimates of nest predation risk, abandonment risk, and exclosure effects that were estimated for your site
       in the", span(strong("Nest Fate Analysis")), "section and plug them into a stochastic population projection model in order
       to predict population growth rate at your site with and without the use of exclosures. A stochastic poulation projection model
       is one that predicts future population trends while incorporating a degree of randomness or unpredictability that is inherent in natural systems,"),
      p("After you have analyzed your nest fate data, a plot appears in this section that displays the mean predicted
        population growth rate with and without exclosures. This plot is a violin plot, which depicts the means
        as black diamonds, and the area shaded in gray represents the relative probability of each outcome -
        the wider the gray area is at a given growth rate, the more frequently that growth rate occurred during
        the model simulations. The plots may have long tails that represent extreme but unlikely growth rates.
        The horizontal bar in the graph indicates the location of growth rate = 1, in other words a stationary
        population; values that appear above this line indicate a growing population, while values below this
        line indicate a declining population. "),
      img(src="lambdaPlot.png"),
      h3("Making a Decision"),
      p("Below the plot is a table that summarizes the outcomes of the population growth predictions; it displays
       the probabilities of rapid growth (growth > 5% per year  ), any growth, any decline, and rapid decline (decline > 5% per year)
        as a function of exclosure use.  These are the main decision criteria. In general, if exclosure use results in increased
         growth probability and decreased decline probability, especially in a population where the predicted growth rate without
          exclosures is below 1.0 or the probability of a decline is high the decision should be to use exclosures. However, the user
          will need to consider their own logistics and values when using these probabilities for decision making - for instance,
          if probability of growth with exclosures is only 10% greater than not using exclosures, it might not be worth the effort for
          some sites with difficult access issues."),
      p("It  is also important to recognize that the population projection model underlying these
        outcomes does not account for site carrying capacity or dispersal rates, in large part because
        these factors are unknown. If the site population is predicted to grow as a result of management,
        extra birds might disperse to other parts of the region if the modeled site is already at carrying
        capacity - thus site-level abundance may stay the same even if the site's population growth rate is >1.0.
        This model therefore predicts the capacity for growth in the absence of density dependence.  Future
        versions of this model might incorporate site-specific carrying capacity, or region-level effects of
        a site-specific management decision."),

      #Abandonment Tolerance section
      h2("Abandonment and Predation Tolerance"),
      p("IMPORTANT: The models in this section use values from
        your", span(strong("Nest Fate Analysis")), ", thus
        you need to complete those sections first before using this one."),
      h2("Abandonment Tolerance"),
      p("If you have chosen to use exclosures, the", span(strong("Abandonment Tolerance")), "section provides
        guidance on how many total abandonments your site can handle before you need to reassess or reverse your
        decision, given that conditions might change unexpectedly or might not match the previous year's conditions.
        Abandonment tolerance is estimated by simulating an increasing baseline level of
        nest abandonment risk while keeping predation risk constant and compares the resulting probability
        of population decline to the probability of decline if exclosures were not used. The adult mortality risks
        is set at the expert opinion level of 70% per observed abandonment, with or without exclosures. This assumption
        means that for each abandonment, the probability that it resulted from the mortality of one of the adults is 70%, and the
        probability that it is a true abandonment (the adults are both alive) is 30%"),
      br(),
      p(strong("NOTE: The number of observed abandonments is for the WHOLE season; if your pre-season assessment
               indicted you could tolerate 2 abandonments, and then your mid-season assessment indicates you can
               tolerate 3 abandonments, this does NOT mean you can tolerate a total of 5 abandonments for the whole
               season"), style="color:red"),
      br(),
      p("An important assumption of this model is that whatever is causing exclosure-related abandonments
        is not also targeting unexclosed nests. For instance, if a raptor is killing adults at exclosures and
        causing abandonments, the model assumes that it is not also preying on plovers at unexclosed nests.
        Such a scenario is not currently accommodated in this model but may be developed in future versions."),
      p("To proceed, press
        the button labeled", span(code("Calculate Threshold", style="color:black")), ". A
        status bar will appear in the bottom left of
        the screen with text stating", span(code("Calculating Abandonment Thresholds", style="color:black")), ". This may take a few minutes,
        after which a plot will appear underneath the", span(strong("Abandonment Threshold Plot")), "heading."),
      img(src="abanThresholdPlot.png"),
      h3("Predation Tolerance"),
      p("If you have chosen not to use exclosures, the", span(strong("Predation Tolerance")), "section provides guidance on
        how many total nest predations your site can handle before you need to reassess or reverse your
        decision, given that conditions might change unexpectedly or might not match the previous year's
        conditions. Predation tolerance is estimated by simulating an increasing level of predation risk
        while keeping abandonment risk constant (with mortality risk set at the expert opinion level of
        70% per observed abandonment, with or without exclosures), and compares the resulting probability
        of population decline to the probability of decline if exclosures were used. It is important to note
         that the predation threshold only applies for predators that would be thwarted by a nest exclosure.
         If some or all of the observed depredations are the results of predators such as ghost crab or mink,
          they should not be counted toward the threshold, as exclosures would not prevent them."),
      br(),
      p("To proceed, press the button labeled", span(code("Calculating Threshold", style="color:black")), "underneath the heading.",
        "A status bar will appear in the bottom left of the screen with text
        stating", span(code("Calculating Predation Thresholds", style="color:black")), "This may take a few minutes, after which a plot will
        appear underneath the", span(strong("Predation Threshold Plot")), "heading."),
      img(src="predThresholdPlot.png"),
      h3("Interpretation"),
      p("The thresholds plots will have a curve with cross-hatching underneath, which represents the
        probability of population decline as a function of observed exclosure-related abandonments or
        nest predations. The dashed horizontal line represents the reference condition of exclosure use
        or no exclosures, depending on the plot. Where the two lines intersect is the point at which
        probability of decline with and without exclosures is identical; this number will also appear
        in the text below the plot as",
        span(code("Reassess or pull exclosures after X observed nest abandonments", style="color:black")),
        "or", span(code("Reassess or pull exclosures after X observed nest predations",style="color:black")), "."),
      p("If you observe the threshold number of abandonments at your site, work through the following flow chart to
        decide whether you should pull exclosures or reassess your site condition by analyzing new data."),
      img(src="thresholdFlowchart.png"),

      #Scenario Modeling
      h2("Scenario Modeling"),
      p("The", span("Scenario Modeling", style="color:blue"), "tab provides users a way to explore hypothetical scenarios.
        This is useful for users in charge of small sites who lack sufficient data to run the previous analyses,
        or for anybody to explore effects of hypothetical scenarios, for example:"),
      p("1. Effects of alternative management actions - if you believe a change in predator control practices might
        reduce the predation risk of unexclosed nests, how will that affect your decision?"),
      p("2. Effects of increased adult mortality risk - if you have a raptor known to target adult plovers at
        exclosures, you can explore a worst case scenario in which abandonment is 100% indicative of adult mortality."),
      p("3. Effects of reduced chick survival - chick survival in the population projection model is set to an
        average value of 0.4 (e.g., 40% of chicks survive from hatch to fledge). If your site likely experiences
        lower or higher chick survival, you can change this value to see how it might affect your results."),
      p("There are four sliders that allow users to change values for predation risk without exclosures,
        abandonment risk with exclosures, probability of an adult mortality given nest abandonment,
        and chick survival. The default values are set to Atlantic Coast-wide average values for predation
        and abandonment based on our 2015 and 2016 analyses. The default mortality risk is based on
        the average of four expert opinion estimates, and the default chick survival is based on
        Atlantic Coast-wide estimates from the literature. If you wish to import values from your previous
        analysis under the", span("Decision Support", style="color:blue"), "tab, check the box marked",
        span(code("Import from Decision Support Analysis", style="color:black")), ". This will move the slider values to match those of your
        site, as well as importing other hidden parameters such as flooding risk and the effects of
        exclosures on predation. To change the slider values, click on the gray circle on the slider
        and move it to your desired value. Once you have selected your values, click the button
        labelled", span(code("Press to Calculate", style="color:black")), ". If at any time you want to reset the values to their default
        settings, click the", span(code("Reset to Default Values", style="color:black")), "button, and also uncheck
        the", span(code("Import from Decision Support Analysis", style="color:black")), "box."),
      p("The model will run for up to 30 seconds, and once it is completed the results will appear on the screen.
        To the right of the sliders a graph will appear showing predicted growth rates with and without exclosures.
        Below the sliders, the table will be populated with probabilities of rapid growth (>5% annually), any growth,
        any decline, and rapid decline (>5% annually), along with the relative effects of exclosure use on these
        probabilities."),
      img(src="scenModelExample.png"),
      img(src="scenModelExample2.png"),
      img(src="scenModelExample3.png"),
      p("For discussion on how to interpret these values and come to a decision, please see the section
        above titled", span(strong("Making a Decision")),".The", span("Scenario Modeling", style="color:blue"), "tab
        includes an", span(strong("Abandonment and Predation Tolerance")),"section to calculate how many
        nest abandonments a site can sustain before the benefits of exclosures do not exceed the risks (or vice-versa)
        based on the values for predation, abandonment, and mortality rated that the user has chosen;
        for details on use and interpretation of this section, please refer to the section above
        called ", span(strong("Abandonment Tolerance")),". 16. For modeling a scenario in which e.g.,
        a Peregrine Falcon is known to be targeting nests,", span(strong("we recommend
        that Mortality Probability Given Abandonment be set at 100%.", style="color:red"))),
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
        starting # of pairs, and the abandonment and predation tolerance plots. Sections not run are left blank in the report or
        populated with NAs. Reports for from the", span("Scenario Modeling", style="color:blue"), "tab include the
        values of predation, abandonment, and mortality risk chosen, population modeling plot and trajectory
        probabilities, starting # of pairs, and the abandonment and predation tolerance plots."),
      br(),
      br()
    )#intructions panel

               )#tabsetPanel
             )) #shinyUI
