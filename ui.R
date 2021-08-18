library(shinythemes)
library(shinydashboard)
library(shiny)
library(shinythemes)
library(bootstrap)
library(shinycustomloader)
library(htmltools)
library(htmlwidgets)
library(DT)
library(ggplot2)
library(slideview)
library(shinycssloaders)
library(shinyWidgets)
library(leaflet)
library(network)
library(igraph)
library(googleformr)
library(ggnetwork)
library(shinyanimate)
ui<-navbarPage(title = "IMMUNE DB",theme = shinytheme("cerulean"),
               tabPanel("HOME",icon = icon("home"),
                        setBackgroundImage(src = "covid1.jpg"),br(),br(),tags$h2(tags$b("IMMPROVE IMMUNE POWER FIGHT WITH PATHOGENES."),align="center"),br(),br(),tags$h3(tags$b("Immune system is the one which fights against foreign particles when they entered into the host body.The immune system
                                                                                   protects us like a soldier from the foreign particle entering into our body.It is very difficult to imaging our entire system without immune system,
                                                                                   our body ia a open book for the foreign particle such as bacteria, fungi,viruses ete..The imune system always identify the
                                                                                    foreign particle and trie to inhibits growth of the foreign particle.Our immune system always completely working the 
                                                                                   lock and key system.Our immune system actually stores the information about pathogen trie to fight against it whenever it enteres.",style="color:black")),
                        br(),br(),br(),br(),br(),img(src="11.png",width="400px",height="300px"),
                        img(src="2.png",width="400px",height="300px"),img(src="3.png",width="400px",height="300px"),
                        img(src="RAW.png",width="100%",height="250px"),tags$h4("TO FND THE LOCATION OF OUR BIRTH PLACE ANY ONE WANT FOOD FOR IMMUNITY I WILL SUPPLY PLESE SEE",style="color:black"),leaflet()%>% addTiles()%>%addMarkers(lng = 79.959869,lat = 15.809432)),
               navbarMenu("IMMUNE SYSTEM",icon = icon("dna"),
                          tabPanel("INNATIVE IMMUNITY",dataTableOutput("tab1"),br(),plotOutput("plot111")),
                          tabPanel("AUTOIMMUNE IMMUNITY",dataTableOutput("tab2"),br(),plotOutput("plot222")),
                          tabPanel("ADAPTIVE IMMUNE",dataTableOutput("tab3"),plotOutput("plot333"))),
               tabPanel("IMMUNE DISEASE",icon = icon("desktop"),
                        navlistPanel("DISEASE TYPE",
                          tabPanel("INNATIVE IMMUNITY",dataTableOutput("tab4"),br(),plotOutput("plot444")),
                          tabPanel("AUTOIMMUNE IMMUNITY",dataTableOutput("tab5"),br(),plotOutput("plot555")),
                          tabPanel("ADAPTIVE IMMUNE",dataTableOutput("tab6"),br(),plotOutput("plot666")))),
               navbarMenu("CONTROL",icon = icon("plus-square"),
                        tabPanel("INNATIVE IMMUNITY",icon = icon("hand-o-right"),dataTableOutput("tab7")),
                        tabPanel("AUTOIMMUNE IMMUNITY",icon = icon("hand-o-right"),dataTableOutput("tab8")),
                        tabPanel("ADAPTIVE IMMUNE",icon = icon("hand-o-right"),dataTableOutput("tab9"))),
               navbarMenu("IMMPROVE IMMUNITY",icon = icon("dashboard"),
               tabPanel("DRUGS TO IMMPROVE IMMUNE",icon = icon("medkit"),dataTableOutput("drug")),
               tabPanel("FOOD TO IMMPROVE IMMUNE",icon = icon("cutlery"),dataTableOutput("food"),br(),plotOutput("foodplot"),br(),plotOutput("foodplot1")),
               tabPanel("PREGNENCY IMMUNITY",icon = icon("female"),tags$iframe(src="preg.pdf",width="100%",height="500px"))),
               tabPanel("GENES",icon = icon("cogs"),
                        tabsetPanel( type = "tabs",id="tab",
                                    tabPanel("INNATIVE IMMUNITY",
                                             navlistPanel("DISEASE NAME",
                                                          tabPanel("Chronic Granulomatous Phagocytic Disease",dataTableOutput("gene1"),br(),br(),plotOutput("plot1")),
                                                          tabPanel("Primary Immune Deficiency",dataTableOutput("gene2"),br(),br(),plotOutput("plot2")),
                                                          tabPanel("Complement Deficiencies",dataTableOutput("gene3"),br(),br(),plotOutput("plot3")),
                                                          tabPanel("Hyper IgE Syndrome",dataTableOutput("gene4"),br(),br(),plotOutput("plot4")),
                                                          tabPanel("Innate Immune Defects",dataTableOutput("gene5"),br(),br(),plotOutput("plot5")),
                                                          tabPanel("NEMO Deficiency Syndrome",dataTableOutput("gene6"),br(),br(),plotOutput("plot6")))),
                                    tabPanel("AUTOIMMUNE IMMUNITY",
                                             navlistPanel("DISEASE NAME",
                                                          tabPanel("Type 1 diabetes",dataTableOutput("gene11"),br(),br(),plotOutput("plot7")),
                                                          tabPanel("Rheumatoid arthritis",dataTableOutput("gene22"),br(),br(),plotOutput("plot8")),
                                                          tabPanel("Psoriasis/psoriatic arthritis",dataTableOutput("gene33"),br(),br(),plotOutput("plot9")),
                                                          tabPanel("Multiple sclerosis",dataTableOutput("gene44"),br(),br(),plotOutput("plot10")),
                                                          tabPanel("Systemic lupus erythematosus",dataTableOutput("gene55"),br(),br(),plotOutput("plot11")),
                                                          tabPanel("Inflammatory bowel disease",dataTableOutput("gene66"),br(),br(),plotOutput("plot12")),
                                                          tabPanel("Addisons disease",dataTableOutput("gene77"),br(),br(),plotOutput("plot13")),
                                                          tabPanel("Graves disease",dataTableOutput("gene88"),br(),br(),plotOutput("plot14")),
                                                          tabPanel("Sjogrens syndrome",dataTableOutput("gene99"),br(),br(),plotOutput("plot15")),
                                                          tabPanel("Hashimotos thyroiditis",dataTableOutput("gene10"),br(),br(),plotOutput("plot16")),
                                                          tabPanel("Myasthenia gravis",dataTableOutput("gene111"),br(),br(),plotOutput("plot17")),
                                                          tabPanel("Autoimmune vasculitis",dataTableOutput("gene12"),br(),br(),plotOutput("plot18")),
                                                          tabPanel("Pernicious anemia",dataTableOutput("gene13"),br(),br(),plotOutput("plot19")),
                                                          tabPanel("Celiac disease",dataTableOutput("gene14"),br(),br(),plotOutput("plot20")))),
                                    tabPanel("ADAPTIVE IMMUNITY",
                                             navlistPanel("DISEASE NAME",
                                                          tabPanel(" Rheumatic diseases",dataTableOutput("gene15"),br(),br(),plotOutput("plot21")))))),
               navbarMenu("TARGETS",icon = icon("users"),
                          tabPanel("INNATIVE IMMUNITY",
                                   tabsetPanel(type = "tabs",
                                               tabPanel("Chronic Granulomatous Phagocytic Disease",dataTableOutput("targ1")),
                                               tabPanel("Primary Immune Deficiency",dataTableOutput("targ2")),
                                               tabPanel("Complement Deficiencies",dataTableOutput("targ3")),
                                               tabPanel("Hyper IgE Syndrome",dataTableOutput("targ4")),
                                               tabPanel("Innate Immune Defects",dataTableOutput("targ5")),
                                               tabPanel("NEMO Deficiency Syndrome",dataTableOutput("targ6")))),
                          tabPanel("AUTOIMMUNE IMMUNITY",
                                   tabsetPanel(type = "tabs",
                                               tabPanel("Type 1 diabetes",dataTableOutput("targ7")),
                                               tabPanel("Rheumatoid arthritis",dataTableOutput("targ8")),
                                               tabPanel("Psoriasis/psoriatic arthritis",dataTableOutput("targ9")),
                                               tabPanel("Multiple sclerosis",dataTableOutput("targ10")),
                                               tabPanel("Systemic lupus erythematosus",dataTableOutput("targ11")),
                                               tabPanel("Inflammatory bowel disease",dataTableOutput("targ12")),
                                               tabPanel("Addisons disease",dataTableOutput("targ13")),
                                               tabPanel("Graves disease",dataTableOutput("targ14")),
                                               tabPanel("Sjogrens syndrome",dataTableOutput("targ15")),
                                               tabPanel("Hashimotos thyroiditis",dataTableOutput("targ16")),
                                               tabPanel("Myasthenia gravis",dataTableOutput("targ17")),
                                               tabPanel("Autoimmune vasculitis",dataTableOutput("targ18")),
                                               tabPanel("Pernicious anemia",dataTableOutput("targ19")),
                                               tabPanel("Celiac disease",dataTableOutput("targ20")))),
                          tabPanel("ADAPTIVE IMMUNITY",
                                   tabsetPanel(type = "tabs",
                                               tabPanel("Rheumatic diseases",dataTableOutput("targ21"))))),
               navbarMenu("NETWORK",icon = icon("line-chart"),
                        tabPanel("INNATIVE IMMUNITY",plotOutput("netplot1")),
                        tabPanel("AUTOIMMUNE IMMUNITY",plotOutput("netplot2")),
                        tabPanel("ADAPTIVE IMMUNE",plotOutput("netplot3"))),
               tabPanel("IMMUNE DEFICENCY AGE",icon = icon("hourglass"),dataTableOutput("age"),plotOutput("ageplot1"),br(),plotOutput("ageplot2"),plotOutput("ageplot3")),
               tabPanel("COVID19",icon = icon("info-circle"),tags$image(src="covid.jpg",height="300px",width="100%"),br(),br(),
                        dataTableOutput("table1"),br(),br(),tags$h3("GENE TABLE",align="center",style="color:yellow"),br(),dataTableOutput("table2"),br(),plotOutput("plot777"),br(),tags$h3("ABSTRACT SEARCH PUBMED",align="center",style="color:black"),br(),dataTableOutput("table3"),br(),plotOutput("plot888"),br(),plotOutput("plot999"),br(),plotOutput("plot1000")),
               navbarMenu("ABSTRACT FIND",icon = icon("flask"),
                                     tabPanel("INNATIVE IMMUNITY",
                                              navlistPanel("DISEASE NAME",
                                                           tabPanel("Chronic Granulomatous Phagocytic Disease",dataTableOutput("abs1"),br(),plotOutput("graph1")),
                                                           tabPanel("Primary Immune Deficiency",dataTableOutput("abs2"),br(),plotOutput("graph2")),
                                                           tabPanel("Complement Deficiencies",dataTableOutput("abs3"),br(),plotOutput("graph3")),
                                                           tabPanel("Hyper IgE Syndrome",dataTableOutput("abs4"),br(),plotOutput("graph4")),
                                                           tabPanel("Innate Immune Defects",dataTableOutput("abs5"),br(),plotOutput("graph5")),
                                                           tabPanel("NEMO Deficiency Syndrome",dataTableOutput("abs6"),br(),plotOutput("graph6")))),
                                     tabPanel("AUTOIMMUNE IMMUNITY",
                                              navlistPanel("DISEASE NAME",
                                                           tabPanel("Type 1 diabetes",dataTableOutput("abs11"),br(),plotOutput("graph7")),
                                                           tabPanel("Rheumatoid arthritis",dataTableOutput("abs22"),br(),plotOutput("graph8")),
                                                           tabPanel("Psoriasis/psoriatic arthritis",dataTableOutput("abs33"),br(),plotOutput("graph9")),
                                                           tabPanel("Multiple sclerosis",dataTableOutput("abs44"),br(),plotOutput("graph10")),
                                                           tabPanel("Systemic lupus erythematosus",dataTableOutput("abs55"),br(),plotOutput("graph11")),
                                                           tabPanel("Inflammatory bowel disease",dataTableOutput("abs66"),br(),plotOutput("graph12")),
                                                           tabPanel("Addisons disease",dataTableOutput("abs77"),br(),plotOutput("graph13")),
                                                           tabPanel("Graves disease",dataTableOutput("abs88"),br(),plotOutput("graph14")),
                                                           tabPanel("Sjogrens syndrome",dataTableOutput("abs99"),br(),plotOutput("graph15")),
                                                           tabPanel("Hashimotos thyroiditis",dataTableOutput("abs10"),br(),plotOutput("graph16")),
                                                           tabPanel("Myasthenia gravis",dataTableOutput("abs111"),br(),plotOutput("graph17")),
                                                           tabPanel("Autoimmune vasculitis",dataTableOutput("abs12"),br(),plotOutput("graph18")),
                                                           tabPanel("Pernicious anemia",dataTableOutput("abs13"),br(),plotOutput("graph19")),
                                                           tabPanel("Celiac disease",dataTableOutput("abs14"),br(),plotOutput("graph20")))),
                                     tabPanel("ADAPTIVE IMMUNITY",
                                              navlistPanel("DISEASE NAME",
                                                           tabPanel(" Rheumatic diseases",dataTableOutput("abs15"),br(),plotOutput("graph21"))))),
               navbarMenu("ABOUT US",icon = icon("phone"),
                          tabPanel("TUTORIALS",icon = icon("file-text-o"),tags$embed(src="tutorial.pdf",width="500px",height="400px"),tags$video(src="vedio.mp4",width=800,height=500,controls="controls")),
                          tabPanel("REFRENCE OF DATABASE",icon = icon("database"),
                                   verticalTabsetPanel(menuSide = "left",
                                                       verticalTabPanel(
                                                         title = "HOME", icon = icon("home", "fa-3x"),
                                                         "The immune system is a complex network of cells and proteins that defends the body against infection. The immune system keeps a record of every germ (microbe) it has ever defeated so it can recognise and destroy the microbe quickly if it enters the body again.
                                                           These database present in the overal information about the adaptive immunity,auto imunity and innative immunity.These different types of immunity to effects the disease and control.
                                                           we collect information of each disease information , genes and argets.Mainly the database present in how to improve immunity power using drugs and food."
                                                       ),
                                                       verticalTabPanel(
                                                         title = "IMMUNE SYSTEM", icon = icon("dna", "fa-3x"),
                                                         "THE IMMUNE SYSTEM OR IMMUNE POWER PRESENT IN THE EVERY HUMAN BODY .THSES IMMUNITY POWER USEFUL MAINLY WHEN ANY PATHOGENIC ORGANISM ENTER
                                              INTO THE BODY THSE IMMUNE POWER AGAINEST TO PRODUCED ANTIBODIES.THSES PROCESS TO PRODECED ANTIBODIES BASED ON THE AGE OF THE PERSON FOR 
                                              EXAMPLE LOWER 15 YEAR AGE PEOPLE IMMUNITY POWER IS LOW EASILY EFFECTS DISEASE AFTER 15 YEAR TO TILL 40 YRARS PEOPLE WORKING IMMUNE SYSTEM 
                                              IS VERY HEIGH.AFTER 40 YEAR AT END OF THE LIFE IMMUNITY POWER IS LOW.THESE IMMUNE SYSTEM CLASSIFY ADAPTIVE IMMUNE SYSTEM,AUTO IMMUNE SYSTEM 
                                              AND INNATIVE IMMUNE SYSTEM .THSEASE IMMUNE SYSTEM ANY PROBLEM TO CAUSING THE VARIOUS DISEASE"
                                                       ),verticalTabPanel(
                                                         title = "IMPROVE IMMUNITY POWER", icon = icon("hand-o-right", "fa-3x"),
                                                         "SOME PEOPLE HAVING THE LESS IMMUNE POWER THOSE PEOPLE EASILY EFFECTS DISEASE THOSE PEOPLE WOULD LOKE TO IMMPROVE IMMUNITY POWER 
                                              IN DATABASE CLEARLY MEANSION FOOD TO IMPROVE IMMUNITY POWER AND DRUGS TO IMPROVE IMMUNITY POWER IN IMMUNE SYSTEM"
                                                       ),verticalTabPanel(
                                                         title = "COVID 19", icon = icon("info-circle", "fa-3x"),
                                                         "TO PROTECT COVID19 FOR WASH THE HAND MULTIPLE TIME AND USING THE MASK TO TALK WITH OTHER 
                                                         PERSON USING SANITISER TO CLEANING HOUSE AND HANDS MULTIPLE TIMES.EVERY DAY DOING BATHING 3-4 TIME HELP FOR REMOVE VIRUSES
                                                         TAKE GOOD FOOD LIKE PROTEIN FOOD,IMMPROVE IMMUNE FOOD",
                                                       ))),
                          tabPanel("CONTACTS",icon = icon("tty"),img(src="1.png",height="300px",width="1000px")),
                          tabPanel("DEVELOPEER",icon = icon("male"),
                                   titlePanel(title="Welcome"),
                                   column(6,
                                          wellPanel(
                                            h2("Hello World"),
                                            br(),
                                            h4("These web application develope by Venu Paritala(7799509079)"))),
                                   column(6,
                                          fluidRow(
                                            wellPanel(
                                              h2("Venu Paritala"),
                                              br(),
                                              h4("Web application developer& Bioinformatician"),
                                              br(),
                                              fluidRow(column(6, 
                                                              h5("I developed web application using the R programming ,HTML,CSS Boot strap and Wordpress")),
                                                       column(6, 
                                                              div(actionButton("button", "button"), style="float:right") 
                                                       )))),
                                          fluidRow(
                                            wellPanel(div(img(src="123.jpg", width="200px",height="200px"), style="text-align: center;")
                                            )))))
)

                                                   
               