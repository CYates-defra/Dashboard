#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Dashboard to display the impact of UADD's data literacy training
#
# To visualise Attendee Characteristics, Feedback Poll data, and Summary Statistics
#
# Written by: Esther Heggie August 2024
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#include changes

# Install libraries
#install.packages("shinydashboard")

# Import libraries
library(shiny)
library(ggplot2)
library(tidyverse)
library(readxl)
library(dplyr)
library(shinydashboard)
library(scales)
library(plotly)
library(mathjaxr)

# Import Data===================================================================
#===============================================================================
# Feedback data-----------------------------------------------------------------
Master_feedback <- read_xlsx("2024_Master_Feedback.xlsx", sheet = "Poll", col_names = TRUE)
TTAS_feedback <- read_xlsx("2024_10_Things_Feedback.xlsx", sheet = "Poll", col_names = TRUE)
WCwD_feedback <- read_xlsx("2024_WCwD_Feedback.xlsx", sheet = "Poll", col_names = TRUE)
DV1_feedback <- read_xlsx("2024_Data_Vis_1_Feedback.xlsx", sheet = "Poll", col_names = TRUE)
DV2_feedback <- read_xlsx("2024_Data_Vis_2_Feedback.xlsx", sheet = "Poll", col_names = TRUE)
QUIZ_feedback <- read_xlsx("2024_QUIZ_Feedback.xlsx", sheet = "Poll", col_names = TRUE)

# Summary Statistics data-------------------------------------------------------
Master_summary_statistics <- read_xlsx("2024_Master_Attendance.xlsx", sheet = "Attendance", col_names = TRUE)
TTAS_summary_statistics <- read_xlsx("2024_10_Things_Attendance.xlsx", sheet = "Attendance", col_names = TRUE)
WCwD_summary_statistics <- read_xlsx("2024_WCwD_Attendance.xlsx", sheet = "Attendance", col_names = TRUE)
DV1_summary_statistics <- read_xlsx("2024_Data_Vis_1_Attendance.xlsx", sheet = "Attendance", col_names = TRUE)
DV2_summary_statistics <- read_xlsx("2024_Data_Vis_2_Attendance.xlsx", sheet = "Attendance", col_names = TRUE)
QUIZ_summary_statistics <- read_xlsx("2024_QUIZ_Attendance.xlsx", sheet = "Attendance", col_names = TRUE)

# Attendee Characteristics data-------------------------------------------------
Master_attendee_characteristics <- read_xlsx("Master attendance data 2024 and 2025.xlsx", sheet = "Master Data", col_names = TRUE)
TTAS_attendee_characteristics <- Master_attendee_characteristics[Master_attendee_characteristics$ID == "10 Things You Need to Know About Statistics"]
WCwD_attendee_characteristics <- Master_attendee_characteristics[Master_attendee_characteristics$ID == "Working Confidently with Data"]
DV1_attendee_characteristics <- Master_attendee_characteristics[Master_attendee_characteristics$ID == "Data Visualisation 1"]
DV2_attendee_characteristics <- Master_attendee_characteristics[Master_attendee_characteristics$ID == "Data Visualisation 2"]
QUIZ_attendee_characteristics <- Master_attendee_characteristics[Master_attendee_characteristics$ID == "QUIZ!! Understanding Analytical Guidance"]


# User Interface================================================================
#===============================================================================
ui <- dashboardPage(
  
  skin = "green",
  dashboardHeader(title = "Impact of Training"),
  
  # Creates side bar to select which training you want to look into
  dashboardSidebar(
    sidebarMenu(
      style = "white-space: normal;",  # Makes side bar text stay in side bar
      
      # Page names
      menuItem("Introduction", tabName = "Introduction"),
      menuItem("Defra Demographics", tabName = "Defra_demographics"),
      menuItem("All Courses", tabName = "All_Courses"),
      menuItem("10 Things You Need to Know About Statistics", tabName = "10_Things"),
      menuItem("Working Confidently with Data", tabName = "WCwD"),
      menuItem("Introduction to Data Visualisation : Part 1", tabName = "data_vis1"),
      menuItem("Introduction to Data Visualisation : Part 2", tabName = "data_vis2"),
      menuItem("QUIZ! Understanding Analytical Guidance", tabName = "barbie_quiz")
    )),
  
  # Creates main dashboard body
  dashboardBody(
    
    tabItems(
      
      # Introduction page=======================================================
      tabItem(tabName = "Introduction",
              fluidPage(
                # Introduction to dashboard
                titlePanel("Welcome to the Impact of Training dashboard")), 
              fluidRow(
                
                column(4, 
                       tags$p("This app has been produced by the Use of Analysis and Data
                     in Decision making (UADD) team."),
                       
                       tags$p("It contains data for the attendee characteristics, 
                     feedback polls, and summary statistics from each of the 
                     training session run by UADD. (Personal data is not used). 
                     For one of our training sessions we also collect the slido 
                     data from the quiz element of the training."),
                       
                       tags$strong("UADD TRAINING - SESSION DESCRIPTIONS"), 
                       
                       br(),
                       
                       tags$strong("10 Things You Need to Know about Statistics"),
                       
                       tags$p("This session presents common pitfalls and issues 
                         around understanding and communicating data. It will 
                         help you build your skills in:"), 
                       tags$ul(
                         tags$li("interpreting statistical evidence, "),
                         tags$li("flagging and understanding potential issues, and "),
                         tags$li("thinking about effective ways to explain and 
                            present findings to a decision maker. ")
                       ),
                       tags$p("Intended audience: Non-specialists who want to feel 
                         more confident in understanding and drawing together 
                         statistical evidence. This course is particularly 
                         relevant to those in the policy profession and those 
                         who need to interpret data as part of their role.  "),
                       
                       tags$strong("Working Confidently with Data"),
                       
                       tags$p("An introduction to data literacy and why it’s important. 
                         This session will explain how to find meaning in numbers 
                         (understanding stories and quality) and help you 
                         communicate data insights effectively."),
                       
                       tags$p("Intended audience: Non-specialists who want to feel 
                         more confident understanding and talking about data."),
                       
                       tags$strong("Introduction to Data Visualisation"),
                       
                       tags$p("This session introduces five principles to follow when 
                         using data to inform decisions. They will help you 
                         develop effective data visualisations that convey your 
                         messages clearly and with impact."),
                       
                       tags$p("Intended audience: Non-specialists who use data in 
                         their work, whether developing visualisations themselves 
                         or commissioning them from others. Also, specialists 
                         looking for a refresher in presenting analysis impactfully."),
                       
                       tags$strong("QUIZ! Understanding Analytical Guidance"),
                       
                       tags$p("A fun and engaging way to understand and learn more 
                         about analytical guidance and good practice.  It covers 
                         both cross-Government and Defra-specific guidance. "),
                       
                       tags$p("Intended audience: Defra group staff who are members 
                         of an analytical profession or anyone who wants to 
                         understand more about analytical guidance and good practice. "),
                       
                       tags$strong("Eligibility for all training sessions:"), 
                       
                       tags$p("All Defra group staff."),
                       
                       tags$strong("IMPORTANT NOTE:"),
                       
                       tags$p("During the process of creating this dashboard (August 2024), 
                     the registration form and attendance data collection for our 
                     training has been improved and standardised for the dashboard 
                     to use. Data collected prior to this has a slightly lower 
                     data quality but it's overall effect on how we measure the 
                     impact of training will decrease as we run the sessions more."),
                       
                       tags$strong("Formulas used in the dashboard"),
                       
                       uiOutput("formulas"),
                       
                       tags$strong("How to use this app:"),
                       
                       tags$ul(
                         tags$li("Make the side-bar appear and disappear using the 
                            three lines at the top."),
                         tags$li("Use the side bar to select which page to view, i.e. 
                            which training."),
                         tags$li("Use the tab headers along the top of the page to 
                            view different areas of analysis of impact of 
                            training for each training."),
                         tags$li("If you use any statistics or charts from this 
                            dashboard in your work, please reference this tool 
                            as: UADD Training Impact dashboard and the date on 
                            which you extracted the data."),
                       ),
                       
                       tags$strong("Important information:"), 
                       
                       tags$p("Data for 2024 is not complete as we update after a 
                         training session is run. "), 
                       
                       tags$strong("If you have any questions about this dashboard, 
                              please contact:"), 
                       
                       tags$ul(
                         tags$li("UADD team: UADD@defra.gov.uk"),
                         tags$li("Hannah Bateman: Hannah.Bateman@defra.gov.uk"),
                       ),
                       
                       tags$p("App built by: Esther Heggie, August 2024"),
                ),
                
                column(width = 8, imageOutput("defra_logo")),
                
              ),
              
              
              
      ),
      
      # Defra Demographics======================================================
      tabItem(tabName = "Defra_demographics", 
              fluidPage(
                titlePanel("Defra Demographics")), 
              fluidRow(
                
                tags$strong("Proportion of attendance to Core Defra headcount"),
                
                tags$p("These number were taken from Civil Service Statistics 
                        bulletin (from the excel sheet attached to Figure 3.3 then table 20)
                       on 20/08/2024 and have been coded as static numbers. 
                       These will needed to be updated as and when the statistical bulletin is updated."),
                
                uiOutput("headcount"),
                
                tags$p("Data caveats:"),
                
                tags$ul(
                  tags$li("The data won't capture the movement of Core Defra 
                          employees that leave after undertaking training"), 
                  tags$li("The data doesn't account for the movement of employees 
                          within Core Defar between grades"), 
                  tags$li("The grades of Core Defra and ALBs do not align, 
                          Grade 6s and 7s are the other way around. Therefore, 
                          this slightly impacts the data quality when analysising 
                          attendee characteristics for each training type"),
                  tags$li("Our training data for grades has been grouped in the 
                          same way at the statistical bulletin for the donut 
                          graphs below for better comparisons"),
                  tags$li("This doesn't account for employees undertaking 
                          multiple trainings. Since we do not use personal ID 
                          data, we can't track whether someone is being tracked 
                          multiple times."),
                ),
                
                
                tags$p("We will use the donut graphs below to monitor whether 
                       the proportion of each grade in attendance at our training 
                       is representative of the proportion of Defra headcount 
                       that the grade makes up."),
                
                h3("Proportion of each grade that makes up attendees per training type"),
                box(title = "Grade proportion of Core Defra", 
                    background = "light-blue", solidHeader = TRUE,  plotlyOutput("Master_prop_grade_donut"),),
                box(title = "10 Things You Need to Know about Statistics", 
                    background = "teal", solidHeader = TRUE,  plotlyOutput("TTAS_prop_grade_donut"),),
                box(title = "Working Confidently with Data", 
                    background = "teal", solidHeader = TRUE,  plotlyOutput("WCwD_prop_grade_donut"),),
                box(title = "Introduction to Data Visualisations : Part 1", 
                    background = "teal", solidHeader = TRUE,  plotlyOutput("DV1_prop_grade_donut"),),
                box(title = "Introduction to Data Visualisations : Part 2", 
                    background = "teal", solidHeader = TRUE,  plotlyOutput("DV2_prop_grade_donut"),),
                box(title = "QUIZ! Understanding Analytical Guidance", 
                    background = "teal", solidHeader = TRUE,  plotlyOutput("QUIZ_prop_grade_donut"),),
              ),
      ),
      
      # All Courses=============================================================
      tabItem(tabName = "All_Courses",
              fluidPage(
                titlePanel("Feedback Polls from All Courses")),
              tabsetPanel(
                # Attendee Characteristics tab----------------------------------
                tabPanel("Attendee Characteristics", 
                         fluidRow(
                           box(title = "Grade Analysis", background = "teal", 
                               solidHeader = TRUE, 
                               actionButton("grade_caveat1", "Show data caveat"),
                               plotOutput("Master_grade")),
                           box(title = "DG Group Analysis", background = "light-blue", 
                               solidHeader = TRUE, 
                               actionButton("DG_caveat1", "Show data caveat"),
                               plotOutput("Master_DG_group")),
                           box(title = "Profession Analysis", background = "light-blue", 
                               solidHeader = TRUE, 
                               actionButton("profession_caveat1", "Show data caveat"),
                               plotOutput("Master_profession")),
                           box(title = "ALB Analysis", background = "teal", 
                               solidHeader = TRUE, 
                               actionButton("ALB_caveat1", "Show data caveat"), 
                               plotOutput("Master_ALB")),
                         ),
                         
                ), 
                
                
                # Feedback Poll data analysis tab
                tabPanel("Feedback Poll", 
                         fluidRow(
                           valueBoxOutput("Master_num_responses"),
                           valueBoxOutput("Master_feedback_response_rate"),
                           box(title = "Did you find this session useful?", background = "teal", 
                               solidHeader = TRUE, plotOutput("Master_useful_plot")),
                           box(title = "Did you find this session engaging?", background = "light-blue", 
                               solidHeader = TRUE, plotOutput("Master_engaging_plot")),
                           box(title = "Did you learn something from this session?", background = "light-blue", 
                               solidHeader = TRUE, plotOutput("Master_learning_plot")),
                           box(title = "Would you recommend this session to a colleague", background = "teal", 
                               solidHeader = TRUE, plotOutput("Master_recommend_plot")),
                         ),
                ),
                
                # Summary Statistics tab----------------------------------------
                tabPanel("Summary Statistics",
                         fluidRow(
                           valueBoxOutput("Master_prop_attend_box"),
                           valueBoxOutput("Master_num_attendees_box"),
                           valueBoxOutput("Master_num_registrations_box"),
                           valueBoxOutput("Master_prop_register_box"),
                           valueBoxOutput("Master_num_page_views_box"),
                           valueBoxOutput("Master_num_cancel_box"),
                           valueBoxOutput("Master_avg_attend_time_box"),
                           valueBoxOutput("Master_train_duration_box"),
                           valueBoxOutput("Master_meet_duration_box"),
                         ),
                ),
              ), 
      ), 
      
      # 10 Things page==========================================================
      tabItem(tabName = "10_Things",
              fluidPage(
                titlePanel("10 Things You Need to Know about Statistics")),
              tabsetPanel(
                # Attendee Characteristics tab----------------------------------
                tabPanel("Attendee Characteristics", 
                         fluidRow(
                           box(title = "Grade Analysis", background = "teal", 
                               solidHeader = TRUE, 
                               actionButton("grade_caveat2", "Show data caveat"),
                               plotOutput("TTAS_grade")),
                           box(title = "DG Group Analysis", background = "light-blue", 
                               solidHeader = TRUE, 
                               actionButton("DG_caveat2", "Show data caveat"),
                               plotOutput("TTAS_DG_group")),
                           box(title = "Profession Analysis", background = "light-blue", 
                               solidHeader = TRUE, 
                               actionButton("profession_caveat2", "Show data caveat"),
                               plotOutput("TTAS_profession")),
                           box(title = "ALB Analysis", background = "teal", 
                               solidHeader = TRUE, 
                               actionButton("ALB_caveat2", "Show data caveat"),
                               plotOutput("TTAS_ALB")),
                         ),
                ),
                
                # Feedback Polls tab--------------------------------------------
                tabPanel("Feedback Polls", 
                         fluidRow(
                           valueBoxOutput("TTAS_num_responses"),
                           valueBoxOutput("TTAS_feedback_response_rate"),
                           box(title = "Did you find this session useful?", background = "teal", 
                               solidHeader = TRUE, plotOutput("TTAS_useful_plot")),
                           box(title = "Did you find this session engaging?", background = "light-blue", 
                               solidHeader = TRUE, plotOutput("TTAS_engaging_plot")),
                           box(title = "Did you learn something from this session?", background = "light-blue", 
                               solidHeader = TRUE, plotOutput("TTAS_learning_plot")),
                           box(title = "Would you recommend this session to a colleague", background = "teal", 
                               solidHeader = TRUE, plotOutput("TTAS_recommend_plot")),
                         ),
                ),
                
                # Summary Statistics tab----------------------------------------
                tabPanel("Summary Statistics",
                         fluidRow(
                           valueBoxOutput("TTAS_prop_attend_box"),
                           valueBoxOutput("TTAS_num_attendees_box"),
                           valueBoxOutput("TTAS_num_registrations_box"),
                           valueBoxOutput("TTAS_prop_register_box"),
                           valueBoxOutput("TTAS_num_page_views_box"),
                           valueBoxOutput("TTAS_num_cancel_box"),
                           valueBoxOutput("TTAS_avg_attend_time_box"),
                           valueBoxOutput("TTAS_train_duration_box"),
                           valueBoxOutput("TTAS_meet_duration_box"),
                         ),
                ),
              ), 
      ), 
      
      # WCwD page===============================================================
      tabItem(tabName = "WCwD",
              fluidPage(
                titlePanel("Working Confidently with Data")), 
              tabsetPanel(
                # Attendee Characteristics tab----------------------------------
                tabPanel("Attendee Characteristics", 
                         fluidRow(
                           box(title = "Grade Analysis", background = "teal", 
                               solidHeader = TRUE, 
                               actionButton("grade_caveat3", "Show data caveat"),
                               plotOutput("WCwD_grade")),
                           box(title = "DG Group Analysis", background = "light-blue", 
                               solidHeader = TRUE, 
                               actionButton("DG_caveat3", "Show data caveat"),
                               plotOutput("WCwD_DG_group")),
                           box(title = "Profession Analysis", background = "light-blue", 
                               solidHeader = TRUE, 
                               actionButton("profession_caveat3", "Show data caveat"),
                               plotOutput("WCwD_profession")),
                           box(title = "ALB Analysis", background = "teal", 
                               solidHeader = TRUE, 
                               actionButton("ALB_caveat3", "Show data caveat"),
                               plotOutput("WCwD_ALB")),
                         ),
                ),
                
                # Feedback Polls tab--------------------------------------------
                tabPanel("Feedback Polls", 
                         fluidRow(
                           valueBoxOutput("WCwD_num_responses"),
                           valueBoxOutput("WCwD_feedback_response_rate"),
                           box(title = "Did you find this session useful?", background = "teal", 
                               solidHeader = TRUE, plotOutput("WCwD_useful_plot")),
                           box(title = "Did you find this session engaging?", background = "light-blue", 
                               solidHeader = TRUE, plotOutput("WCwD_engaging_plot")),
                           box(title = "Did you learn something from this session?", background = "light-blue", 
                               solidHeader = TRUE, plotOutput("WCwD_learning_plot")),
                           box(title = "Would you recommend this session to a colleague", background = "teal", 
                               solidHeader = TRUE, plotOutput("WCwD_recommend_plot")),
                         ),
                ),
                
                # Summary Statistics tab----------------------------------------
                tabPanel("Summary Statistics",
                         fluidRow(
                           valueBoxOutput("WCwD_prop_attend_box"),
                           valueBoxOutput("WCwD_num_attendees_box"),
                           valueBoxOutput("WCwD_num_registrations_box"),
                           valueBoxOutput("WCwD_prop_register_box"),
                           valueBoxOutput("WCwD_num_page_views_box"),
                           valueBoxOutput("WCwD_num_cancel_box"),
                           valueBoxOutput("WCwD_avg_attend_time_box"),
                           valueBoxOutput("WCwD_train_duration_box"),
                           valueBoxOutput("WCwD_meet_duration_box"),
                         ),
                ),
              ), 
      ),
      
      # Data vis 1 page=========================================================
      tabItem(tabName = "data_vis1",
              fluidPage(
                titlePanel("Introduction to Data Visualisations : Part 1")), 
              tabsetPanel(
                # Attendee Characteristics tab----------------------------------
                tabPanel("Attendee Characteristics", 
                         fluidRow(
                           box(title = "Grade Analysis", background = "teal", 
                               solidHeader = TRUE, 
                               actionButton("grade_caveat4", "Show data caveat"),
                               plotOutput("DV1_grade")),
                           box(title = "DG Group Analysis", background = "light-blue", 
                               solidHeader = TRUE, 
                               actionButton("DG_caveat4", "Show data caveat"),
                               plotOutput("DV1_DG_group")),
                           box(title = "Profession Analysis", background = "light-blue", 
                               solidHeader = TRUE, 
                               actionButton("profession_caveat4", "Show data caveat"),
                               plotOutput("DV1_profession")),
                           box(title = "ALB Analysis", background = "teal", 
                               solidHeader = TRUE, 
                               actionButton("ALB_caveat4", "Show data caveat"),
                               plotOutput("DV1_ALB")),
                         ),
                ),
                
                # Feedback Polls tab--------------------------------------------
                tabPanel("Feedback Polls", 
                         fluidRow(
                           valueBoxOutput("DV1_num_responses"),
                           valueBoxOutput("DV1_feedback_response_rate"),
                           box(title = "Did you find this session useful?", background = "teal", 
                               solidHeader = TRUE, plotOutput("DV1_useful_plot")),
                           box(title = "Did you find this session engaging?", background = "light-blue", 
                               solidHeader = TRUE, plotOutput("DV1_engaging_plot")),
                           box(title = "Did you learn something from this session?", background = "light-blue", 
                               solidHeader = TRUE, plotOutput("DV1_learning_plot")),
                           box(title = "Would you recommend this session to a colleague", background = "teal", 
                               solidHeader = TRUE, plotOutput("DV1_recommend_plot")),
                         ),
                ),
                
                # Summary Statistics tab----------------------------------------
                tabPanel("Summary Statistics",
                         fluidRow(
                           valueBoxOutput("DV1_prop_attend_box"),
                           valueBoxOutput("DV1_num_attendees_box"),
                           valueBoxOutput("DV1_num_registrations_box"),
                           valueBoxOutput("DV1_prop_register_box"),
                           valueBoxOutput("DV1_num_page_views_box"),
                           valueBoxOutput("DV1_num_cancel_box"),
                           valueBoxOutput("DV1_avg_attend_time_box"),
                           valueBoxOutput("DV1_train_duration_box"),
                           valueBoxOutput("DV1_meet_duration_box"),
                         ),
                ),
              ), 
      ),
      
      # Data vis 2 page=========================================================
      tabItem(tabName = "data_vis2",
              fluidPage(
                titlePanel("Introduction to Data Visualisations : Part 2")), 
              tabsetPanel(
                # Attendee Characteristics tab----------------------------------
                tabPanel("Attendee Characteristics", 
                         fluidRow(
                           box(title = "Grade Analysis", background = "teal", 
                               solidHeader = TRUE, 
                               actionButton("grade_caveat5", "Show data caveat"),
                               plotOutput("DV2_grade")),
                           box(title = "DG Group Analysis", background = "light-blue", 
                               solidHeader = TRUE, 
                               actionButton("DG_caveat5", "Show data caveat"),
                               plotOutput("DV2_DG_group")),
                           box(title = "Profession Analysis", background = "light-blue", 
                               solidHeader = TRUE, 
                               actionButton("profession_caveat5", "Show data caveat"),
                               plotOutput("DV2_profession")),
                           box(title = "ALB Analysis", background = "teal", 
                               solidHeader = TRUE, 
                               actionButton("ALB_caveat5", "Show data caveat"),
                               plotOutput("DV2_ALB")),
                         ),
                ),
                
                # Feedback Polls tab--------------------------------------------
                tabPanel("Feedback Polls",
                         fluidRow(
                           valueBoxOutput("DV2_num_responses"),
                           valueBoxOutput("DV2_feedback_response_rate"),
                           box(title = "Did you find this session useful?", background = "teal", 
                               solidHeader = TRUE, plotOutput("DV2_useful_plot")),
                           box(title = "Did you find this session engaging?", background = "light-blue", 
                               solidHeader = TRUE, plotOutput("DV2_engaging_plot")),
                           box(title = "Did you learn something from this session?", background = "light-blue", 
                               solidHeader = TRUE, plotOutput("DV2_learning_plot")),
                           box(title = "Would you recommend this session to a colleague", background = "teal", 
                               solidHeader = TRUE, plotOutput("DV2_recommend_plot")),
                         ),
                ),
                
                # Summary Statistics tab----------------------------------------
                tabPanel("Summary Statistics",
                         fluidRow(
                           valueBoxOutput("DV2_prop_attend_box"),
                           valueBoxOutput("DV2_num_attendees_box"),
                           valueBoxOutput("DV2_num_registrations_box"),
                           valueBoxOutput("DV2_prop_register_box"),
                           valueBoxOutput("DV2_num_page_views_box"),
                           valueBoxOutput("DV2_num_cancel_box"),
                           valueBoxOutput("DV2_avg_attend_time_box"),
                           valueBoxOutput("DV2_train_duration_box"),
                           valueBoxOutput("DV2_meet_duration_box"),
                         ),
                ),
              ), 
      ),
      
      # Barbie quiz page========================================================
      tabItem(tabName = "barbie_quiz",
              fluidPage(
                titlePanel("QUIZ! Understanding Analytical Guidance")), 
              tabsetPanel(
                # Attendee Characteristics tab----------------------------------
                tabPanel("Attendee Characteristics", 
                         fluidRow(
                           box(title = "Grade Analysis", background = "teal", 
                               solidHeader = TRUE, 
                               actionButton("grade_caveat6", "Show data caveat"),
                               plotOutput("QUIZ_grade")),
                           box(title = "DG Group Analysis", background = "light-blue", 
                               solidHeader = TRUE, 
                               actionButton("DG_caveat6", "Show data caveat"),
                               plotOutput("QUIZ_DG_group")),
                           box(title = "Profession Analysis", background = "light-blue", 
                               solidHeader = TRUE, 
                               actionButton("profession_caveat6", "Show data caveat"),
                               plotOutput("QUIZ_profession")),
                           box(title = "ALB Analysis", background = "teal", 
                               solidHeader = TRUE, 
                               actionButton("ALB_caveat6", "Show data caveat"),
                               plotOutput("QUIZ_ALB")),
                         ),
                ),
                
                # Feedback Polls tab--------------------------------------------
                tabPanel("Feedback Polls", 
                         fluidRow(
                           valueBoxOutput("QUIZ_num_responses"),
                           valueBoxOutput("QUIZ_feedback_response_rate"),
                           box(title = "Did you find this session useful?", background = "teal", 
                               solidHeader = TRUE, plotOutput("QUIZ_useful_plot")),
                           box(title = "Did you find this session engaging?", background = "light-blue", 
                               solidHeader = TRUE, plotOutput("QUIZ_engaging_plot")),
                           box(title = "Did you learn something from this session?", background = "light-blue", 
                               solidHeader = TRUE, plotOutput("QUIZ_learning_plot")),
                           box(title = "Would you recommend this session to a colleague", background = "teal", 
                               solidHeader = TRUE, plotOutput("QUIZ_recommend_plot")),
                         ),
                ),
                
                # Summary Statistics tab----------------------------------------
                tabPanel("Summary Statistics",
                         fluidRow(
                           valueBoxOutput("QUIZ_prop_attend_box"),
                           valueBoxOutput("QUIZ_num_attendees_box"),
                           valueBoxOutput("QUIZ_num_registrations_box"),
                           valueBoxOutput("QUIZ_prop_register_box"),
                           valueBoxOutput("QUIZ_num_page_views_box"),
                           valueBoxOutput("QUIZ_num_cancel_box"),
                           valueBoxOutput("QUIZ_avg_attend_time_box"),
                           valueBoxOutput("QUIZ_train_duration_box"),
                           valueBoxOutput("QUIZ_meet_duration_box"),
                         ),
                ),
                
                # Slido data tab
                tabPanel("Slido data", 
                         fluidRow(
                           
                         )),
              ), 
      )
    )  # end of tab items
  ),  # end of dashboard body
)  # end of ui bracket




# Server Logic==================================================================
#===============================================================================
server <- function(input, output) {
  
  # Functions used within code 
  # RAG rating for value boxes (because they only accept certain colours)
  RAG <- function(value){
    if (value <= 0.5){
      RAG = "maroon"
    }
    else if (value < 0.75){
      RAG = "orange"
    }
    else {
      RAG = "olive"
    }
  }
  
  # Doughnut graph code with inbuilt RAG rating
  big_number_donut_plot <- function(title, value) {
    
    # RAG rating (using DEFRA style guide colours)
    RAG_donut <- function(value){
      if (value <= 0.5){
        RAG_donut = "#BD0A1B"
      }
      else if (value < 0.75){
        RAG_donut = "#FFCC00"
      }
      else {
        RAG_donut = "#77BC1F"
      }
    }
    
    # Wrangle data to get a data frame in the format we need it in to make our donut chart
    df <- tibble(x = 1, y = value) %>% 
      mutate(y_negative = 1 - y) %>% 
      pivot_longer(cols = -x) 
    
    # Create a nicely formatted big number to go in the donut hole
    big_number_text_label <- percent(value, accuracy = 1)
    
    # Create our plot
    ggplot(df,
           aes(x = x,
               y = value,
               fill = name)) +
      
      # Add a bar, but don't add the legend
      geom_col(show.legend = FALSE) +
      
      # Add title
      ggtitle(label = title) +
      
      # A pie/donut chart is a bar chart with polar coordinates
      # Add polar coordinates and set the direction to -1 
      # so the filled in part starts at the top and goes clockwise
      coord_polar(theta = "y",
                  direction = -1) +
      
      
      # Set the limits, which is important for adding the hole
      xlim(c(-2, 2)) +
      
      # Set a color scale with the highlighted section in whatever color
      # is chosen with the highlight_color argument and the rest in a light gray
      scale_fill_manual(values = c(RAG_donut(value), "grey90")) +
      
      # Set theme_void() to remove grid lines and everything else from the plot
      theme_void() +
      
      # Add the big number in the center of the hole
      annotate("text",
               label = big_number_text_label,
               family = "Serif",
               fontface = "bold",
               color = RAG_donut(value),
               size = 12,
               x = -2,
               y = 0)
    
  }
  
  
  
  # Data caveats================================================================
  # Grade Analysis
  observeEvent(input$grade_caveat1 | input$grade_caveat2 | input$grade_caveat3 |
                 input$grade_caveat4 | input$grade_caveat5 | input$grade_caveat6, {
                   showModal(modalDialog(
                     title = "Grade data caveat", 
                     "Grades are defined differently between DG Groups and ALBs. This difference
      is that Grade 6s and Grade 7s are the opposite way round to DG Groups in ALBs.", 
                     easyClose = TRUE, footer = NULL
                   ))
                 })
  
  # DG Group Analysis
  observeEvent(input$DG_caveat1 | input$DG_caveat2 | input$DG_caveat3 |
                 input$DG_caveat4 | input$DG_caveat5 | input$DG_caveat6, {
                   showModal(modalDialog(
                     title = "DG Group data caveat",
                     "Since we are unable to root questions in the registration form, some ALB
      attendees select a DG Group rather than N/A when filling it in.",
                     easyClose = TRUE, footer = NULL
                   ))
                 })
  
  # Profession Analysis
  observeEvent(input$profession_caveat1 | input$profession_caveat2 | input$profession_caveat3 |
                 input$profession_caveat4 | input$profession_caveat5 | input$profession_caveat6, {
                   showModal(modalDialog(
                     title = "Profession data caveat",
                     "ALB attendees won't have specific professions. However, due to being
      unable to root questions in the registration form, some ALB attendees might
      answer this question with a profession rather than clickly the N/A option.",
                     easyClose = TRUE, footer = NULL
                   ))
                 })
  
  # ALB Analysis
  observeEvent(input$ALB_caveat1 | input$ALB_caveat2 | input$ALB_caveat3 |
                 input$ALB_caveat4 | input$ALB_caveat5 | input$ALB_caveat6, {
                   showModal(modalDialog(
                     title = "ALB data caveat",
                     "The collection of ALB data was not standardised until August 2024.
      Data collected before the standardisation of the registration form will be
      missing data entries that were not inputted in the way the code reads it.",
                     easyClose = TRUE, footer = NULL
                   ))
                 })
  
  
  
  
  # Introduction================================================================
  output$formulas <- renderUI({
    withMathJax(
      helpText('Feedback response rate: $$\\frac{Number of responses}{Number in attendance} * 100$$'), 
      helpText('Attendees as a proportion of those who registered: $$\\frac{Number of attendees}{Number of registrations} * 100$$'), 
      helpText('Proportion of people who viewed registration page that then signed up: $$\\frac{Number of registrations}{Number of page views} * 100$$')
    )
  })
  
  output$defra_logo <- renderImage({
    list(src = "Defra-logo.png", 
         width = 300, height = 200)
  }, deleteFile = F)
  
  
  # DEFRA DEMOGRAPHICS==========================================================
  url <- a("Civil Service statistical bulletin", 
           href="https://www.gov.uk/government/statistics/civil-service-statistics-2023/statistical-bulletin-civil-service-statistics-2023#grade")
  output$headcount <- renderUI({tagList("Link to data source:", url)})
  
  # Grade proportion analysis by training type
  # Master
  output$Master_prop_grade_donut <- renderPlotly({
    # Donut graph of Grade 
    # create a data frame with category labels and corresponding values
    data <- data.frame(
      category = c("AO/AA", "EO", "HEO/SEO", "Grade 7/Grade 6", "SCS"),
      value = c(2540, 2240, 5030, 2990, 235)
    )
    
    # define the colors for each category
    colors <- c("#E6F2F8", "#CADAE4", "#8DA8BF", "#44729A", "#01538A")
    
    # set the marker properties, including the colors and line width
    marker <- list(colors = colors)
    
    # create the pie chart with a hole in the center
    plot_ly(data, labels = ~category, values = ~value, type = "pie",
            hole = 0.5, marker = marker)
  }) 
  
  # TTAS
  output$TTAS_prop_grade_donut <- renderPlotly({
    # Donut graph of Grade 
    # create a data frame with category labels and corresponding values
    data <- data.frame(
      category = c("AO/AA", "EO", "HEO/SEO", "Grade 7/Grade 6", "SCS"),
      value = c(length(which(TTAS_attendee_characteristics[TTAS_attendee_characteristics$"What is your Civil Service grade?"] == "AO/AA")),
                length(which(TTAS_attendee_characteristics[TTAS_attendee_characteristics$"What is your Civil Service grade?"] == "EO")),
                length(which(TTAS_attendee_characteristics[TTAS_attendee_characteristics$"What is your Civil Service grade?"] == "HEO")) + length(which(TTAS_attendee_characteristics[TTAS_attendee_characteristics$"What is your Civil Service grade?"] == "SEO")),
                length(which(TTAS_attendee_characteristics[TTAS_attendee_characteristics$"What is your Civil Service grade?"] == "Grade 7")) + length(which(TTAS_attendee_characteristics[TTAS_attendee_characteristics$"What is your Civil Service grade?"] == "Grade 6")),
                length(which(TTAS_attendee_characteristics[TTAS_attendee_characteristics$"What is your Civil Service grade?"] == "SCS")))
    )
    
    # define the colors for each category
    colors <- c("#E6F2F8", "#CADAE4", "#8DA8BF", "#44729A", "#01538A")
    
    # set the marker properties, including the colors and line width
    marker <- list(colors = colors)
    
    # create the pie chart with a hole in the center
    plot_ly(data, labels = ~category, values = ~value, type = "pie",
            hole = 0.5, marker = marker)
  })
  
  # WCwD
  output$WCwD_prop_grade_donut <- renderPlotly({
    # Donut graph of Grade 
    # create a data frame with category labels and corresponding values
    data <- data.frame(
      category = c("AO/AA", "EO", "HEO/SEO", "Grade 7/Grade 6", "SCS"),
      value = c(length(which(WCwD_attendee_characteristics[WCwD_attendee_characteristics$"What is your Civil Service grade?"] == "AO/AA")),
                length(which(WCwD_attendee_characteristics[WCwD_attendee_characteristics$"What is your Civil Service grade?"] == "EO")),
                length(which(WCwD_attendee_characteristics[WCwD_attendee_characteristics$"What is your Civil Service grade?"] == "HEO")) + length(which(WCwD_attendee_characteristics[WCwD_attendee_characteristics$"What is your Civil Service grade?"] == "SEO")),
                length(which(WCwD_attendee_characteristics[WCwD_attendee_characteristics$"What is your Civil Service grade?"] == "Grade 7")) + length(which(WCwD_attendee_characteristics[WCwD_attendee_characteristics$"What is your Civil Service grade?"] == "Grade 6")),
                length(which(WCwD_attendee_characteristics[WCwD_attendee_characteristics$"What is your Civil Service grade?"] == "SCS")))
    )
    
    # define the colors for each category
    colors <- c("#E6F2F8", "#CADAE4", "#8DA8BF", "#44729A", "#01538A")
    
    # set the marker properties, including the colors and line width
    marker <- list(colors = colors)
    
    # create the pie chart with a hole in the center
    plot_ly(data, labels = ~category, values = ~value, type = "pie",
            hole = 0.5, marker = marker)
  })
  
  # DV1
  output$DV1_prop_grade_donut <- renderPlotly({
    # Donut graph of Grade 
    # create a data frame with category labels and corresponding values
    data <- data.frame(
      category = c("AO/AA", "EO", "HEO/SEO", "Grade 7/Grade 6", "SCS"),
      value = c(length(which(DV1_attendee_characteristics[DV1_attendee_characteristics$"What is your Civil Service grade?"] == "AO/AA")),
                length(which(DV1_attendee_characteristics[DV1_attendee_characteristics$"What is your Civil Service grade?"] == "EO")),
                length(which(DV1_attendee_characteristics[DV1_attendee_characteristics$"What is your Civil Service grade?"] == "HEO")) + length(which(DV1_attendee_characteristics[DV1_attendee_characteristics$"What is your Civil Service grade?"] == "SEO")),
                length(which(DV1_attendee_characteristics[DV1_attendee_characteristics$"What is your Civil Service grade?"] == "Grade 7")) + length(which(DV1_attendee_characteristics[DV1_attendee_characteristics$"What is your Civil Service grade?"] == "Grade 6")),
                length(which(DV1_attendee_characteristics[DV1_attendee_characteristics$"What is your Civil Service grade?"] == "SCS")))
    )
    
    # define the colors for each category
    colors <- c("#E6F2F8", "#CADAE4", "#8DA8BF", "#44729A", "#01538A")
    
    # set the marker properties, including the colors and line width
    marker <- list(colors = colors)
    
    # create the pie chart with a hole in the center
    plot_ly(data, labels = ~category, values = ~value, type = "pie",
            hole = 0.5, marker = marker)
  })
  
  # DV2
  output$DV2_prop_grade_donut <- renderPlotly({
    # Donut graph of Grade 
    # create a data frame with category labels and corresponding values
    data <- data.frame(
      category = c("AO/AA", "EO", "HEO/SEO", "Grade 7/Grade 6", "SCS"),
      value = c(length(which(DV2_attendee_characteristics[DV2_attendee_characteristics$"What is your Civil Service grade?"] == "AO/AA")),
                length(which(DV2_attendee_characteristics[DV2_attendee_characteristics$"What is your Civil Service grade?"] == "EO")),
                length(which(DV2_attendee_characteristics[DV2_attendee_characteristics$"What is your Civil Service grade?"] == "HEO")) + length(which(DV2_attendee_characteristics[DV2_attendee_characteristics$"What is your Civil Service grade?"] == "SEO")),
                length(which(DV2_attendee_characteristics[DV2_attendee_characteristics$"What is your Civil Service grade?"] == "Grade 7")) + length(which(DV2_attendee_characteristics[DV2_attendee_characteristics$"What is your Civil Service grade?"] == "Grade 6")),
                length(which(DV2_attendee_characteristics[DV2_attendee_characteristics$"What is your Civil Service grade?"] == "SCS")))
    )
    
    # define the colors for each category
    colors <- c("#E6F2F8", "#CADAE4", "#8DA8BF", "#44729A", "#01538A")
    
    # set the marker properties, including the colors and line width
    marker <- list(colors = colors)
    
    # create the pie chart with a hole in the center
    plot_ly(data, labels = ~category, values = ~value, type = "pie",
            hole = 0.5, marker = marker)
  })
  
  # QUIZ
  output$QUIZ_prop_grade_donut <- renderPlotly({
    # Donut graph of Grade 
    # create a data frame with category labels and corresponding values
    data <- data.frame(
      category = c("AO/AA", "EO", "HEO/SEO", "Grade 7/Grade 6", "SCS"),
      value = c(length(which(QUIZ_attendee_characteristics[QUIZ_attendee_characteristics$"What is your Civil Service grade?"] == "AO/AA")),
                length(which(QUIZ_attendee_characteristics[QUIZ_attendee_characteristics$"What is your Civil Service grade?"] == "EO")),
                length(which(QUIZ_attendee_characteristics[QUIZ_attendee_characteristics$"What is your Civil Service grade?"] == "HEO")) + length(which(QUIZ_attendee_characteristics[QUIZ_attendee_characteristics$"What is your Civil Service grade?"] == "SEO")),
                length(which(QUIZ_attendee_characteristics[QUIZ_attendee_characteristics$"What is your Civil Service grade?"] == "Grade 7")) + length(which(QUIZ_attendee_characteristics[QUIZ_attendee_characteristics$"What is your Civil Service grade?"] == "Grade 6")),
                length(which(QUIZ_attendee_characteristics[QUIZ_attendee_characteristics$"What is your Civil Service grade?"] == "SCS")))
    )
    
    # define the colors for each category
    colors <- c("#E6F2F8", "#CADAE4", "#8DA8BF", "#44729A", "#01538A")
    
    # set the marker properties, including the colors and line width
    marker <- list(colors = colors)
    
    # create the pie chart with a hole in the center
    plot_ly(data, labels = ~category, values = ~value, type = "pie",
            hole = 0.5, marker = marker)
  })
  
  
  
  
  # ALL COURSES=================================================================
  # Attendee Characteristics----------------------------------------------------
  # Grade analysis 
  output$Master_grade <- renderPlot({
    x = c("AO/AA", "EO", "HEO", "SEO", "Grade 7", "Grade 6", "SCS")
    
    y = c(length(which(Master_attendee_characteristics[Master_attendee_characteristics$"What is your Civil Service grade?"] == "AO/AA")),
          length(which(Master_attendee_characteristics[Master_attendee_characteristics$"What is your Civil Service grade?"] == "EO")),
          length(which(Master_attendee_characteristics[Master_attendee_characteristics$"What is your Civil Service grade?"] == "HEO")),
          length(which(Master_attendee_characteristics[Master_attendee_characteristics$"What is your Civil Service grade?"] == "SEO")),
          length(which(Master_attendee_characteristics[Master_attendee_characteristics$"What is your Civil Service grade?"] == "Grade 7")),
          length(which(Master_attendee_characteristics[Master_attendee_characteristics$"What is your Civil Service grade?"] == "Grade 6")),
          length(which(Master_attendee_characteristics[Master_attendee_characteristics$"What is your Civil Service grade?"] == "SCS")))
    
    # Create data frame
    df <- data.frame(x, y)
    
    # Column plot results
    ggplot(df, aes(x = fct_inorder(x, ordered = NA), y = y)) + geom_col(fill = "navy") + 
      xlab("Grade") + ylab("Number in attendance") + theme(axis.text = element_text(size = 14), 
                                                           axis.title = element_text(size = 14, face = "bold"), 
                                                           plot.title = element_text(size = 20))
  })
  
  # DG group analysis
  output$Master_DG_group <- renderPlot({
    x = c("Environment", "Food, Biosecurity, and Trade", "Portfolio Delivery", 
          "Strategy", "Science and Analysis")
    
    y = c(length(which(Master_attendee_characteristics[Master_attendee_characteristics$"If you’re from Core Defra, which DG group are you part of? (please select “N/A” if you are from an ALB)"] == "Environment")),
          length(which(Master_attendee_characteristics[Master_attendee_characteristics$"If you’re from Core Defra, which DG group are you part of? (please select “N/A” if you are from an ALB)"] == "Food, Biosecurity, and Trade")),
          length(which(Master_attendee_characteristics[Master_attendee_characteristics$"If you’re from Core Defra, which DG group are you part of? (please select “N/A” if you are from an ALB)"] == "Portfolio Delivery")),
          length(which(Master_attendee_characteristics[Master_attendee_characteristics$"If you’re from Core Defra, which DG group are you part of? (please select “N/A” if you are from an ALB)"] == "Strategy")),
          length(which(Master_attendee_characteristics[Master_attendee_characteristics$"If you’re from Core Defra, which DG group are you part of? (please select “N/A” if you are from an ALB)"] == "Science and Analysis")))
    
    # Create data frame
    df <- data.frame(x, y)
    
    # Column plot results
    ggplot(df, aes(x = reorder(fct_inorder(x, ordered = NA), y), y = y)) +
      geom_bar(stat = "identity", fill = "navy") + coord_flip() + xlab("DG Group") +
      ylab("Number in attendance") + theme(axis.text = element_text(size = 14), 
                                           axis.title = element_text(size = 14, face = "bold"), 
                                           plot.title = element_text(size = 20))
  })
  
  # Profession analysis
  output$Master_profession <- renderPlot({
    x = c("Operational Delivery",
          "Policy",
          "Commercial",
          "Communications",
          "Digital and Data",
          "Economics",
          "Finance",
          "Geography",
          "Human Resources",
          "Project Delivery",
          "Property",
          "Operational Research",
          "Social Research",
          "Statistics",
          "Legal",
          "Science and Engineering",
          "Veterinary",
          "Other")
    
    y = c(length(which(Master_attendee_characteristics[Master_attendee_characteristics$"What is your profession?"] == "Operational Delivery")),
          length(which(Master_attendee_characteristics[Master_attendee_characteristics$"What is your profession?"] == "Policy")),
          length(which(Master_attendee_characteristics[Master_attendee_characteristics$"What is your profession?"] == "Commercial")),
          length(which(Master_attendee_characteristics[Master_attendee_characteristics$"What is your profession?"] == "Communications")),
          length(which(Master_attendee_characteristics[Master_attendee_characteristics$"What is your profession?"] == "Digital and Data")),
          length(which(Master_attendee_characteristics[Master_attendee_characteristics$"What is your profession?"] == "Economics")),
          length(which(Master_attendee_characteristics[Master_attendee_characteristics$"What is your profession?"] == "Finance")),
          length(which(Master_attendee_characteristics[Master_attendee_characteristics$"What is your profession?"]== "Geography")),
          length(which(Master_attendee_characteristics[Master_attendee_characteristics$"What is your profession?"] == "Human Resources")),
          length(which(Master_attendee_characteristics[Master_attendee_characteristics$"What is your profession?"] == "Project Delivery")),
          length(which(Master_attendee_characteristics[Master_attendee_characteristics$"What is your profession?"] == "Property")),
          length(which(Master_attendee_characteristics[Master_attendee_characteristics$"What is your profession?"] == "Operational Research")),
          length(which(Master_attendee_characteristics[Master_attendee_characteristics$"What is your profession?"] == "Social Research")),
          length(which(Master_attendee_characteristics[Master_attendee_characteristics$"What is your profession?"] == "Statistics")),
          length(which(Master_attendee_characteristics[Master_attendee_characteristics$"What is your profession?"] == "Legal")),
          length(which(Master_attendee_characteristics[Master_attendee_characteristics$"What is your profession?"] == "Science and Engineering")),
          length(which(Master_attendee_characteristics[Master_attendee_characteristics$"What is your profession?"] == "Veterinary")),
          length(which(Master_attendee_characteristics[Master_attendee_characteristics$"What is your profession?"] == "Other")))
    
    # Create data frame
    df <- data.frame(x, y)
    
    # Column plot results
    ggplot(df, aes(x = reorder(fct_inorder(x, ordered = NA), y), y = y)) + 
      geom_bar(stat = "identity", fill = "navy") + coord_flip() + xlab("Profession") +
      ylab("Number in attendance") + theme(axis.text = element_text(size = 14), 
                                           axis.title = element_text(size = 14, face = "bold"), 
                                           plot.title = element_text(size = 20))
  })
  
  # ALB Analysis
  output$Master_ALB <- renderPlot({
    x = c("Animal and Plant Health Agency", 
          "Centre for Environment, Fisheries, and Aquaculture Science",
          "Environment Agency", "Forestry Commission", "Marine Management Organisation", 
          "Natural England", "Rural Payments Agency", "Veterinary Medicines Directorate",
          "Other", "N/A")
    
    y = c(length(which(Master_attendee_characteristics[Master_attendee_characteristics$"Which department do you belong to?"] == "Animal and Plant Health Agenc")),
          length(which(Master_attendee_characteristics[Master_attendee_characteristics$"Which department do you belong to?"] == "Centre for Environment, Fisheries, and Aquaculture Science")),
          length(which(Master_attendee_characteristics[Master_attendee_characteristics$"Which department do you belong to?"] == "Environment Agency")),
          length(which(Master_attendee_characteristics[Master_attendee_characteristics$"Which department do you belong to?"] == "Forestry Commission")),
          length(which(Master_attendee_characteristics[Master_attendee_characteristics$"Which department do you belong to?"] == "Marine Management Organisation")),
          length(which(Master_attendee_characteristics[Master_attendee_characteristics$"Which department do you belong to?"] == "Natural England")),
          length(which(Master_attendee_characteristics[Master_attendee_characteristics$"Which department do you belong to?"] == "Rural Payments Agency")),
          length(which(Master_attendee_characteristics[Master_attendee_characteristics$"Which department do you belong to?"] == "Veterinary Medicines Directorate")),
          length(which(Master_attendee_characteristics[Master_attendee_characteristics$"Which department do you belong to?"] == "Other")),
          length(which(Master_attendee_characteristics[Master_attendee_characteristics$"Which department do you belong to?"] == "N/A")))
    
    # Create data frame
    df <- data.frame(x, y)
    
    # Column plot results
    ggplot(df, aes(x = reorder(fct_inorder(x, ordered = NA), y), y = y)) + geom_bar(stat = "identity", fill = "navy") + coord_flip() + 
      xlab("ALB") + ylab("Number in attendance") + theme(axis.text = element_text(size = 14), 
                                                         axis.title = element_text(size = 14, face = "bold"), 
                                                         plot.title = element_text(size = 20))
  })
  
  
  
  
  # Feedback Polls -------------------------------------------------------------
  # Number of responses = number of rows             
  master_num_responses <- function(){nrow(Master_feedback)}
  output$Master_num_responses <- renderValueBox({
    valueBox(paste0(master_num_responses()), "Number of responses", color = "olive")})
  
  # Feedback response rate - 
  master_feedback_response_rate <- function(){
    round(((nrow(Master_feedback)) / sum(Master_summary_statistics[4])) * 100, digits = 0)}
  output$Master_feedback_response_rate <- renderValueBox({
    valueBox(paste0(master_feedback_response_rate(), "%"), "Feedback response rate", color = RAG(master_feedback_response_rate() / 100))})
  
  # QUESTION: Did you find this session useful?
  output$Master_useful_plot <- renderPlot({
    # Create x vector to five survey response options
    x = c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")
    
    # Create y vector that counts the number of each response type
    y = c(length(which(Master_feedback[6] == "Strongly Disagree")) + length(which(Master_feedback[6] == "Strongly disagree")),
          length(which(Master_feedback[6] == "Disagree")),
          length(which(Master_feedback[6] == "Neutral")),
          length(which(Master_feedback[6] == "Agree")),
          length(which(Master_feedback[6] == "Strongly Agree")) + length(which(Master_feedback[6] == "Strongly agree")))
    
    # Create data frame
    df <- data.frame(x, y)
    
    # Column plot results
    ggplot(df, aes(x = fct_inorder(x, ordered = NA), y = y)) + geom_col(fill = "navy") +
      ggtitle(paste("[", sum(y), " responses]")) +
      geom_text(aes(label = y), vjust = -0.2) +
      ylab("Number of response") + theme(axis.title.x = element_blank(), 
                                         axis.text = element_text(size = 14), 
                                         axis.title = element_text(size = 14, face = "bold"), 
                                         plot.title = element_text(size = 20)) 
  })
  
  #QUESTION: Did you find this session engaging?
  output$Master_engaging_plot <- renderPlot({
    # Create x vector to five survey response options
    x = c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")
    
    # Create y vector that counts the number of each response type
    y = c(length(which(Master_feedback[7] == "Strongly Disagree")) + length(which(Master_feedback[7] == "Strongly disagree")),
          length(which(Master_feedback[7] == "Disagree")),
          length(which(Master_feedback[7] == "Neutral")),
          length(which(Master_feedback[7] == "Agree")),
          length(which(Master_feedback[7] == "Strongly Agree")) + length(which(Master_feedback[7] == "Strongly agree")))
    
    # Create data frame
    df <- data.frame(x, y)
    
    # Column plot results
    ggplot(df, aes(x = fct_inorder(x, ordered = NA), y = y)) + geom_col(fill = "navy") + 
      ggtitle(paste("[", sum(y), " responses]")) +
      geom_text(aes(label = y), vjust = -0.2) +
      ylab("Number of response") + theme(axis.title.x = element_blank(), 
                                         axis.text = element_text(size = 14), 
                                         axis.title = element_text(size = 14, face = "bold"), 
                                         plot.title = element_text(size = 20))
  })
  
  #QUESTION: Did you learn something from this session?
  output$Master_learning_plot <- renderPlot({
    # Create x vector to five survey response options
    x = c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")
    
    # Create y vector that counts the number of each response type
    y = c(length(which(Master_feedback[8] == "Strongly Disagree")) + length(which(Master_feedback[8] == "Strongly disagree")),
          length(which(Master_feedback[8] == "Disagree")),
          length(which(Master_feedback[8] == "Neutral")),
          length(which(Master_feedback[8] == "Agree")),
          length(which(Master_feedback[8] == "Strongly Agree")) + length(which(Master_feedback[8] == "Strongly agree")))
    
    # Create data frame
    df <- data.frame(x, y)
    
    # Column plot results
    ggplot(df, aes(x = fct_inorder(x, ordered = NA), y = y)) + geom_col(fill = "navy") +
      ggtitle(paste("[", sum(y), " responses]")) + 
      geom_text(aes(label = y), vjust = -0.2) +
      ylab("Number of response") + theme(axis.title.x = element_blank(), 
                                         axis.text = element_text(size = 14), 
                                         axis.title = element_text(size = 14, face = "bold"), 
                                         plot.title = element_text(size = 20))
  })
  
  #QUESTION: Would you recommend this session to a colleague
  output$Master_recommend_plot <- renderPlot({
    # Create x vector to five survey response options
    x = c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")
    
    # Create y vector that counts the number of each response type
    y = c(length(which(Master_feedback[9] == "Strongly Disagree")) + length(which(Master_feedback[9] == "Strongly disagree")),
          length(which(Master_feedback[9] == "Disagree")),
          length(which(Master_feedback[9] == "Neutral")),
          length(which(Master_feedback[9] == "Agree")),
          length(which(Master_feedback[9] == "Strongly Agree")) + length(which(Master_feedback[9] == "Strongly agree")))
    
    # Create data frame
    df <- data.frame(x, y)
    
    # Column plot results
    ggplot(df, aes(x = fct_inorder(x, ordered = NA), y = y)) + geom_col(fill = "navy") + 
      ggtitle(paste("[", sum(y), " responses]")) + 
      geom_text(aes(label = y), vjust = -0.2) +
      ylab("Number of response") + theme(axis.title.x = element_blank(), 
                                         axis.text = element_text(size = 14), 
                                         axis.title = element_text(size = 14, face = "bold"), 
                                         plot.title = element_text(size = 20))
  })
  
  
  # Summary Statistics----------------------------------------------------------
  # Attendees as a proportion of those who registered                                        
  prop_attend <- function(){
    round((as.numeric(sum(Master_summary_statistics[4])) / 
             as.numeric(sum(Master_summary_statistics[2]))) * 100, digits = 0)}
  output$Master_prop_attend_box <- renderValueBox({
    valueBox(paste0(prop_attend(), "%"), 
             "Attendees as a proportion of those who registered", color = RAG(prop_attend() / 100))})
  
  # Number of attendees
  num_attendees <- function(){sum(Master_summary_statistics[4])}
  output$Master_num_attendees_box <- renderValueBox({
    valueBox(paste0(num_attendees()), "Number of attendees", color = "light-blue")})
  
  # Number of registrations
  num_registrations <- function(){sum(Master_summary_statistics[2])}
  output$Master_num_registrations_box <- renderValueBox({
    valueBox(paste0(num_registrations()), "Number of registrations", color = "light-blue")})
  
  # Proportion of people who viewed registration page that then signed up, (num registered + num cancelled) / num views
  prop_register <- function(){
    round(((as.numeric(sum(Master_summary_statistics[2])) + 
              as.numeric(sum(Master_summary_statistics[3]))) / 
             as.numeric(sum(Master_summary_statistics[1]))) * 100, digits = 0)}
  output$Master_prop_register_box <- renderValueBox({
    valueBox(paste0(prop_register(), "%"), 
             "Proportion of people who viewed registration page that then signed up", 
             color = RAG(prop_register() / 100))}) 
  
  # Number of page views
  num_page_views <- function(){sum(Master_summary_statistics[1])}
  output$Master_num_page_views_box <- renderValueBox({
    valueBox(paste0(num_page_views()), "Number of page views", color = "light-blue")})
  
  # Number of cancellations
  num_cancel <- function(){as.numeric(sum(Master_summary_statistics[3]))}
  output$Master_num_cancel_box <- renderValueBox({
    valueBox(paste0(num_cancel()), "Number of cancellations", color = "light-blue")})
  
  
  
  
  # 10 THINGS YOU NEED TO KNOW ABOUT STATISTICS=================================
  # Attendee Characteristics----------------------------------------------------
  # Grade analysis 
  output$TTAS_grade <- renderPlot({
    x = c("AO/AA", "EO", "HEO", "SEO", "Grade 7", "Grade 6", "SCS")
    
    y = c(length(which(TTAS_attendee_characteristics[2] == "AO/AA")),
          length(which(TTAS_attendee_characteristics[2] == "EO")),
          length(which(TTAS_attendee_characteristics[2] == "HEO")),
          length(which(TTAS_attendee_characteristics[2] == "SEO")),
          length(which(TTAS_attendee_characteristics[2] == "Grade 7")),
          length(which(TTAS_attendee_characteristics[2] == "Grade 6")),
          length(which(TTAS_attendee_characteristics[2] == "SCS")))
    
    # Create data frame
    df <- data.frame(x, y)
    
    # Column plot results
    ggplot(df, aes(x = fct_inorder(x, ordered = NA), y = y)) + geom_col(fill = "navy") + 
      xlab("Grade") + ylab("Number in attendance") + theme(axis.text = element_text(size = 14), 
                                                           axis.title = element_text(size = 14, face = "bold"), 
                                                           plot.title = element_text(size = 20))
  })
  
  # DG group analysis
  output$TTAS_DG_group <- renderPlot({
    x = c("Environment", "Food, Biosecurity, and Trade", "Portfolio Delivery", 
          "Strategy", "Science and Analysis")
    
    y = c(length(which(TTAS_attendee_characteristics[3] == "Environment")),
          length(which(TTAS_attendee_characteristics[3] == "Food, Biosecurity, and Trade")),
          length(which(TTAS_attendee_characteristics[3] == "Portfolio Delivery")),
          length(which(TTAS_attendee_characteristics[3] == "Strategy")),
          length(which(TTAS_attendee_characteristics[3] == "Science and Analysis")))
    
    # Create data frame
    df <- data.frame(x, y)
    
    # Column plot results
    ggplot(df, aes(x = reorder(fct_inorder(x, ordered = NA), y), y = y)) +
      geom_bar(stat = "identity", fill = "navy") + coord_flip() + xlab("DG Group") +
      ylab("Number in attendance") + theme(axis.text = element_text(size = 14), 
                                           axis.title = element_text(size = 14, face = "bold"), 
                                           plot.title = element_text(size = 20))
  })
  
  # Profession analysis
  output$TTAS_profession <- renderPlot({
    x = c("Operational Delivery",
          "Policy",
          "Commercial",
          "Communications",
          "Digital and Data",
          "Economics",
          "Finance",
          "Geography",
          "Human Resources",
          "Project Delivery",
          "Property",
          "Operational Research",
          "Social Research",
          "Statistics",
          "Legal",
          "Science and Engineering",
          "Veterinary",
          "Other")
    
    y = c(length(which(TTAS_attendee_characteristics[1] == "Operational Delivery")),
          length(which(TTAS_attendee_characteristics[1] == "Policy")),
          length(which(TTAS_attendee_characteristics[1] == "Commercial")),
          length(which(TTAS_attendee_characteristics[1] == "Communications")),
          length(which(TTAS_attendee_characteristics[1] == "Digital and Data")),
          length(which(TTAS_attendee_characteristics[1] == "Economics")),
          length(which(TTAS_attendee_characteristics[1] == "Finance")),
          length(which(TTAS_attendee_characteristics[1] == "Geography")),
          length(which(TTAS_attendee_characteristics[1] == "Human Resources")),
          length(which(TTAS_attendee_characteristics[1] == "Project Delivery")),
          length(which(TTAS_attendee_characteristics[1] == "Property")),
          length(which(TTAS_attendee_characteristics[1] == "Operational Research")),
          length(which(TTAS_attendee_characteristics[1] == "Social Research")),
          length(which(TTAS_attendee_characteristics[1] == "Statistics")),
          length(which(TTAS_attendee_characteristics[1] == "Legal")),
          length(which(TTAS_attendee_characteristics[1] == "Science and Engineering")),
          length(which(TTAS_attendee_characteristics[1] == "Veterinary")),
          length(which(TTAS_attendee_characteristics[1] == "Other")))
    
    # Create data frame
    df <- data.frame(x, y)
    
    # Column plot results
    ggplot(df, aes(x = reorder(fct_inorder(x, ordered = NA), y), y = y)) + 
      geom_bar(stat = "identity", fill = "navy") + coord_flip() + xlab("Profession") +
      ylab("Number in attendance") + theme(axis.text = element_text(size = 14), 
                                           axis.title = element_text(size = 14, face = "bold"), 
                                           plot.title = element_text(size = 20))
  })
  
  # ALB Analysis
  output$TTAS_ALB <- renderPlot({
    x = c("Animal and Plant Health Agency", 
          "Centre for Environment, Fisheries, and Aquaculture Science",
          "Environment Agency", "Forestry Commission", "Marine Management Organisation", 
          "Natural England", "Rural Payments Agency", "Veterinary Medicines Directorate",
          "Other", "N/A")
    
    y = c(length(which(TTAS_attendee_characteristics[4] == "Animal and Plant Health Agenc")),
          length(which(TTAS_attendee_characteristics[4] == "Centre for Environment, Fisheries, and Aquaculture Science")),
          length(which(TTAS_attendee_characteristics[4] == "Environment Agency")),
          length(which(TTAS_attendee_characteristics[4] == "Forestry Commission")),
          length(which(TTAS_attendee_characteristics[4] == "Marine Management Organisation")),
          length(which(TTAS_attendee_characteristics[4] == "Natural England")),
          length(which(TTAS_attendee_characteristics[4] == "Rural Payments Agency")),
          length(which(TTAS_attendee_characteristics[4] == "Veterinary Medicines Directorate")),
          length(which(TTAS_attendee_characteristics[4] == "Other")),
          length(which(TTAS_attendee_characteristics[4] == "N/A")))
    
    # Create data frame
    df <- data.frame(x, y)
    
    # Column plot results
    ggplot(df, aes(x = reorder(fct_inorder(x, ordered = NA), y), y = y)) + geom_bar(stat = "identity", fill = "navy") + coord_flip() + 
      xlab("ALB") + ylab("Number in attendance") + theme(axis.text = element_text(size = 14), 
                                                         axis.title = element_text(size = 14, face = "bold"), 
                                                         plot.title = element_text(size = 20))
  })
  
  
  # Feedback polls--------------------------------------------------------------
  # Number of responses = number of rows
  TTAS_num_responses <- function(){nrow(TTAS_feedback)}
  output$TTAS_num_responses <- renderValueBox({
    valueBox(paste0(TTAS_num_responses()), "Number of responses", color = "olive")})
  
  # Feedback response rate 
  TTAS_feedback_response_rate <- function(){
    round(((nrow(TTAS_feedback)) / as.numeric(TTAS_summary_statistics[4])) * 100, digits = 0)}
  output$TTAS_feedback_response_rate <- renderValueBox({
    valueBox(paste0(TTAS_feedback_response_rate(), "%"), "Feedback response rate", color = RAG(TTAS_feedback_response_rate() / 100))})
  
  # QUESTION: Did you find this session useful?
  output$TTAS_useful_plot <- renderPlot({
    # Create x vector to five survey response options
    x = c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")
    
    # Create y vector that counts the number of each response type
    y = c(length(which(TTAS_feedback[6] == "Strongly Disagree")) + length(which(TTAS_feedback[6] == "Strongly disagree")),
          length(which(TTAS_feedback[6] == "Disagree")),
          length(which(TTAS_feedback[6] == "Neutral")),
          length(which(TTAS_feedback[6] == "Agree")),
          length(which(TTAS_feedback[6] == "Strongly Agree")) + length(which(TTAS_feedback[6] == "Strongly agree")))
    
    # Create data frame
    df <- data.frame(x, y)
    
    # Column plot results
    ggplot(df, aes(x = fct_inorder(x, ordered = NA), y = y)) + geom_col(fill = "navy") +
      ggtitle(paste("[", sum(y), " responses]")) +
      geom_text(aes(label = y), vjust = -0.2) +
      ylab("Number of response") + theme(axis.title.x = element_blank(), 
                                         axis.text = element_text(size = 14), 
                                         axis.title = element_text(size = 14, face = "bold"), 
                                         plot.title = element_text(size = 20)) 
  })
  
  #QUESTION: Did you find this session engaging?
  output$TTAS_engaging_plot <- renderPlot({
    # Create x vector to five survey response options
    x = c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")
    
    # Create y vector that counts the number of each response type
    y = c(length(which(TTAS_feedback[7] == "Strongly Disagree")) + length(which(TTAS_feedback[7] == "Strongly disagree")),
          length(which(TTAS_feedback[7] == "Disagree")),
          length(which(TTAS_feedback[7] == "Neutral")),
          length(which(TTAS_feedback[7] == "Agree")),
          length(which(TTAS_feedback[7] == "Strongly Agree")) + length(which(TTAS_feedback[7] == "Strongly agree")))
    
    # Create data frame
    df <- data.frame(x, y)
    
    # Column plot results
    ggplot(df, aes(x = fct_inorder(x, ordered = NA), y = y)) + geom_col(fill = "navy") + 
      ggtitle(paste("[", sum(y), " responses]")) +
      geom_text(aes(label = y), vjust = -0.2) +
      ylab("Number of response") + theme(axis.title.x = element_blank(), 
                                         axis.text = element_text(size = 14), 
                                         axis.title = element_text(size = 14, face = "bold"), 
                                         plot.title = element_text(size = 20))
  })
  
  #QUESTION: Did you learn something from this session?
  output$TTAS_learning_plot <- renderPlot({
    # Create x vector to five survey response options
    x = c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")
    
    # Create y vector that counts the number of each response type
    y = c(length(which(TTAS_feedback[8] == "Strongly Disagree")) + length(which(TTAS_feedback[8] == "Strongly disagree")),
          length(which(TTAS_feedback[8] == "Disagree")),
          length(which(TTAS_feedback[8] == "Neutral")),
          length(which(TTAS_feedback[8] == "Agree")),
          length(which(TTAS_feedback[8] == "Strongly Agree")) + length(which(TTAS_feedback[8] == "Strongly agree")))
    
    # Create data frame
    df <- data.frame(x, y)
    
    # Column plot results
    ggplot(df, aes(x = fct_inorder(x, ordered = NA), y = y)) + geom_col(fill = "navy") +
      ggtitle(paste("[", sum(y), " responses]")) + 
      geom_text(aes(label = y), vjust = -0.2) +
      ylab("Number of response") + theme(axis.title.x = element_blank(), 
                                         axis.text = element_text(size = 14), 
                                         axis.title = element_text(size = 14, face = "bold"), 
                                         plot.title = element_text(size = 20))
  })
  
  #QUESTION: Would you recommend this session to a colleague
  output$TTAS_recommend_plot <- renderPlot({
    # Create x vector to five survey response options
    # Create x vector to five survey response options
    x = c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")
    
    # Create y vector that counts the number of each response type
    y = c(length(which(TTAS_feedback[9] == "Strongly Disagree")) + length(which(TTAS_feedback[9] == "Strongly disagree")),
          length(which(TTAS_feedback[9] == "Disagree")),
          length(which(TTAS_feedback[9] == "Neutral")),
          length(which(TTAS_feedback[9] == "Agree")),
          length(which(TTAS_feedback[9] == "Strongly Agree")) + length(which(TTAS_feedback[9] == "Strongly agree")))
    
    # Create data frame
    df <- data.frame(x, y)
    
    # Column plot results
    ggplot(df, aes(x = fct_inorder(x, ordered = NA), y = y)) + geom_col(fill = "navy") + 
      ggtitle(paste("[", sum(y), " responses]")) + 
      geom_text(aes(label = y), vjust = -0.2) +
      ylab("Number of response") + theme(axis.title.x = element_blank(), 
                                         axis.text = element_text(size = 14), 
                                         axis.title = element_text(size = 14, face = "bold"), 
                                         plot.title = element_text(size = 20))
  })
  
  
  # Summary Statistics----------------------------------------------------------
  # Attendees as a proportion of those who registered                                        
  TTAS_prop_attend <- function(){
    round((as.numeric(TTAS_summary_statistics[4]) / 
             as.numeric(TTAS_summary_statistics[2])) * 100, digits = 0)}
  output$TTAS_prop_attend_box <- renderValueBox({
    valueBox(paste0(TTAS_prop_attend(), "%"), 
             "Attendees as a proportion of those who registered", color = RAG(TTAS_prop_attend() / 100))})
  
  # Number of attendees
  TTAS_num_attendees <- function(){TTAS_summary_statistics[4]}
  output$TTAS_num_attendees_box <- renderValueBox({
    valueBox(paste0(TTAS_num_attendees()), "Number of attendees", color = "light-blue")})
  
  # Number of registrations
  TTAS_num_registrations <- function(){TTAS_summary_statistics[2]}
  output$TTAS_num_registrations_box <- renderValueBox({
    valueBox(paste0(TTAS_num_registrations()), "Number of registrations", color = "light-blue")})
  
  # Proportion of people who viewed registration page that then signed up, (num registered + num cancelled) / num views
  TTAS_prop_register <- function(){
    round(((as.numeric(TTAS_summary_statistics[2]) + 
              as.numeric(TTAS_summary_statistics[3])) / 
             as.numeric(TTAS_summary_statistics[1])) * 100, digits = 0)}
  output$TTAS_prop_register_box <- renderValueBox({
    valueBox(paste0(TTAS_prop_register(), "%"), 
             "Proportion of people who viewed registration page that then signed up", 
             color = RAG(TTAS_prop_register() / 100))}) 
  
  # Number of page views
  TTAS_num_page_views <- function(){TTAS_summary_statistics[1]}
  output$TTAS_num_page_views_box <- renderValueBox({
    valueBox(paste0(TTAS_num_page_views()), "Number of page views", color = "light-blue")})
  
  # Number of cancellations
  TTAS_num_cancel <- function(){as.numeric(TTAS_summary_statistics[3])}
  output$TTAS_num_cancel_box <- renderValueBox({
    valueBox(paste0(TTAS_num_cancel()), "Number of cancellations", color = "light-blue")})
  
  # Average attendance time
  TTAS_avg_attend_time <- function(){TTAS_summary_statistics[6]}
  output$TTAS_avg_attend_time_box <- renderValueBox({
    valueBox(paste0(TTAS_avg_attend_time()), "Average attendance duration", color = "light-blue")})
  
  # Training duration
  TTAS_train_duration <- function(){"1h 30m 00s"}
  output$TTAS_train_duration_box <- renderValueBox({
    valueBox(paste0(TTAS_train_duration()), "Target training duration", color = "light-blue")})
  
  # Meeting duration
  TTAS_meet_duration <- function(){TTAS_summary_statistics[5]}
  output$TTAS_meet_duration_box <- renderValueBox({
    valueBox(paste0(TTAS_meet_duration()), "Actual training duration", color = "light-blue")})
  
  
  
  
  # WORKING CONFIDENTLY WITH DATA===============================================
  # Attendee Characteristics----------------------------------------------------
  # Grade analysis 
  output$WCwD_grade <- renderPlot({
    x = c("AO/AA", "EO", "HEO", "SEO", "Grade 7", "Grade 6", "SCS")
    
    y = c(length(which(WCwD_attendee_characteristics[2] == "AO/AA")),
          length(which(WCwD_attendee_characteristics[2] == "EO")),
          length(which(WCwD_attendee_characteristics[2] == "HEO")),
          length(which(WCwD_attendee_characteristics[2] == "SEO")),
          length(which(WCwD_attendee_characteristics[2] == "Grade 7")),
          length(which(WCwD_attendee_characteristics[2] == "Grade 6")),
          length(which(WCwD_attendee_characteristics[2] == "SCS")))
    
    # Create data frame
    df <- data.frame(x, y)
    
    # Column plot results
    ggplot(df, aes(x = fct_inorder(x, ordered = NA), y = y)) + geom_col(fill = "navy") + 
      xlab("Grade") + ylab("Number in attendance") + theme(axis.text = element_text(size = 14), 
                                                           axis.title = element_text(size = 14, face = "bold"), 
                                                           plot.title = element_text(size = 20))
  })
  
  # DG group analysis
  output$WCwD_DG_group <- renderPlot({
    x = c("Environment", "Food, Biosecurity, and Trade", "Portfolio Delivery", 
          "Strategy", "Science and Analysis")
    
    y = c(length(which(WCwD_attendee_characteristics[3] == "Environment")),
          length(which(WCwD_attendee_characteristics[3] == "Food, Biosecurity, and Trade")),
          length(which(WCwD_attendee_characteristics[3] == "Portfolio Delivery")),
          length(which(WCwD_attendee_characteristics[3] == "Strategy")),
          length(which(WCwD_attendee_characteristics[3] == "Science and Analysis")))
    
    # Create data frame
    df <- data.frame(x, y)
    
    # Column plot results
    ggplot(df, aes(x = reorder(fct_inorder(x, ordered = NA), y), y = y)) +
      geom_bar(stat = "identity", fill = "navy") + coord_flip() + xlab("DG Group") +
      ylab("Number in attendance") + theme(axis.text = element_text(size = 14), 
                                           axis.title = element_text(size = 14, face = "bold"), 
                                           plot.title = element_text(size = 20))
  })
  
  # Profession analysis
  output$WCwD_profession <- renderPlot({
    x = c("Operational Delivery",
          "Policy",
          "Commercial",
          "Communications",
          "Digital and Data",
          "Economics",
          "Finance",
          "Geography",
          "Human Resources",
          "Project Delivery",
          "Property",
          "Operational Research",
          "Social Research",
          "Statistics",
          "Legal",
          "Science and Engineering",
          "Veterinary",
          "Other")
    
    y = c(length(which(WCwD_attendee_characteristics[1] == "Operational Delivery")),
          length(which(WCwD_attendee_characteristics[1] == "Policy")),
          length(which(WCwD_attendee_characteristics[1] == "Commercial")),
          length(which(WCwD_attendee_characteristics[1] == "Communications")),
          length(which(WCwD_attendee_characteristics[1] == "Digital and Data")),
          length(which(WCwD_attendee_characteristics[1] == "Economics")),
          length(which(WCwD_attendee_characteristics[1] == "Finance")),
          length(which(WCwD_attendee_characteristics[1] == "Geography")),
          length(which(WCwD_attendee_characteristics[1] == "Human Resources")),
          length(which(WCwD_attendee_characteristics[1] == "Project Delivery")),
          length(which(WCwD_attendee_characteristics[1] == "Property")),
          length(which(WCwD_attendee_characteristics[1] == "Operational Research")),
          length(which(WCwD_attendee_characteristics[1] == "Social Research")),
          length(which(WCwD_attendee_characteristics[1] == "Statistics")),
          length(which(WCwD_attendee_characteristics[1] == "Legal")),
          length(which(WCwD_attendee_characteristics[1] == "Science and Engineering")),
          length(which(WCwD_attendee_characteristics[1] == "Veterinary")),
          length(which(WCwD_attendee_characteristics[1] == "Other")))
    
    # Create data frame
    df <- data.frame(x, y)
    
    # Column plot results
    ggplot(df, aes(x = reorder(fct_inorder(x, ordered = NA), y), y = y)) + 
      geom_bar(stat = "identity", fill = "navy") + coord_flip() + xlab("Profession") +
      ylab("Number in attendance") + theme(axis.text = element_text(size = 14), 
                                           axis.title = element_text(size = 14, face = "bold"), 
                                           plot.title = element_text(size = 20))
  })
  
  # ALB Analysis
  output$WCwD_ALB <- renderPlot({
    x = c("Animal and Plant Health Agency", 
          "Centre for Environment, Fisheries, and Aquaculture Science",
          "Environment Agency", "Forestry Commission", "Marine Management Organisation", 
          "Natural England", "Rural Payments Agency", "Veterinary Medicines Directorate",
          "Other", "N/A")
    
    y = c(length(which(WCwD_attendee_characteristics[4] == "Animal and Plant Health Agenc")),
          length(which(WCwD_attendee_characteristics[4] == "Centre for Environment, Fisheries, and Aquaculture Science")),
          length(which(WCwD_attendee_characteristics[4] == "Environment Agency")),
          length(which(WCwD_attendee_characteristics[4] == "Forestry Commission")),
          length(which(WCwD_attendee_characteristics[4] == "Marine Management Organisation")),
          length(which(WCwD_attendee_characteristics[4] == "Natural England")),
          length(which(WCwD_attendee_characteristics[4] == "Rural Payments Agency")),
          length(which(WCwD_attendee_characteristics[4] == "Veterinary Medicines Directorate")),
          length(which(WCwD_attendee_characteristics[4] == "Other")),
          length(which(WCwD_attendee_characteristics[4] == "N/A")))
    
    # Create data frame
    df <- data.frame(x, y)
    
    # Column plot results
    ggplot(df, aes(x = reorder(fct_inorder(x, ordered = NA), y), y = y)) + geom_bar(stat = "identity", fill = "navy") + coord_flip() + 
      xlab("ALB") + ylab("Number in attendance") + theme(axis.text = element_text(size = 14), 
                                                         axis.title = element_text(size = 14, face = "bold"), 
                                                         plot.title = element_text(size = 20))
  })
  
  
  # Feedback polls--------------------------------------------------------------
  # Number of responses = number of rows
  WCwD_num_responses <- function(){nrow(WCwD_feedback)}
  output$WCwD_num_responses <- renderValueBox({
    valueBox(paste0(WCwD_num_responses()), "Number of responses", color = "olive")})
  
  # Feedback response rate 
  WCwD_feedback_response_rate <- function(){
    round(((nrow(WCwD_feedback)) / as.numeric(WCwD_summary_statistics[4])) * 100, digits = 0)}
  output$WCwD_feedback_response_rate <- renderValueBox({
    valueBox(paste0(WCwD_feedback_response_rate(), "%"), "Feedback response rate", color = RAG(WCwD_feedback_response_rate() / 100))})
  
  # QUESTION: Did you find this session useful?
  output$WCwD_useful_plot <- renderPlot({
    # Create x vector to five survey response options
    x = c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")
    
    # Create y vector that counts the number of each response type
    y = c(length(which(WCwD_feedback[6] == "Strongly Disagree")) + length(which(WCwD_feedback[6] == "Strongly disagree")),
          length(which(WCwD_feedback[6] == "Disagree")),
          length(which(WCwD_feedback[6] == "Neutral")),
          length(which(WCwD_feedback[6] == "Agree")),
          length(which(WCwD_feedback[6] == "Strongly Agree")) + length(which(WCwD_feedback[6] == "Strongly agree")))
    
    # Create data frame
    df <- data.frame(x, y)
    
    # Column plot results
    ggplot(df, aes(x = fct_inorder(x, ordered = NA), y = y)) + geom_col(fill = "navy") +
      ggtitle(paste("[", sum(y), " responses]")) +
      geom_text(aes(label = y), vjust = -0.2) +
      ylab("Number of response") + theme(axis.title.x = element_blank(), 
                                         axis.text = element_text(size = 14), 
                                         axis.title = element_text(size = 14, face = "bold"), 
                                         plot.title = element_text(size = 20)) 
  })
  
  #QUESTION: Did you find this session engaging?
  output$WCwD_engaging_plot <- renderPlot({
    # Create x vector to five survey response options
    x = c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")
    
    # Create y vector that counts the number of each response type
    y = c(length(which(WCwD_feedback[7] == "Strongly Disagree")) + length(which(WCwD_feedback[7] == "Strongly disagree")),
          length(which(WCwD_feedback[7] == "Disagree")),
          length(which(WCwD_feedback[7] == "Neutral")),
          length(which(WCwD_feedback[7] == "Agree")),
          length(which(WCwD_feedback[7] == "Strongly Agree")) + length(which(WCwD_feedback[7] == "Strongly agree")))
    
    # Create data frame
    df <- data.frame(x, y)
    
    # Column plot results
    ggplot(df, aes(x = fct_inorder(x, ordered = NA), y = y)) + geom_col(fill = "navy") + 
      ggtitle(paste("[", sum(y), " responses]")) +
      geom_text(aes(label = y), vjust = -0.2) +
      ylab("Number of response") + theme(axis.title.x = element_blank(), 
                                         axis.text = element_text(size = 14), 
                                         axis.title = element_text(size = 14, face = "bold"), 
                                         plot.title = element_text(size = 20))
  })
  
  #QUESTION: Did you learn something from this session?
  output$WCwD_learning_plot <- renderPlot({
    # Create x vector to five survey response options
    x = c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")
    
    # Create y vector that counts the number of each response type
    y = c(length(which(WCwD_feedback[8] == "Strongly Disagree")) + length(which(WCwD_feedback[8] == "Strongly disagree")),
          length(which(WCwD_feedback[8] == "Disagree")),
          length(which(WCwD_feedback[8] == "Neutral")),
          length(which(WCwD_feedback[8] == "Agree")),
          length(which(WCwD_feedback[8] == "Strongly Agree")) + length(which(WCwD_feedback[8] == "Strongly agree")))
    
    # Create data frame
    df <- data.frame(x, y)
    
    # Column plot results
    ggplot(df, aes(x = fct_inorder(x, ordered = NA), y = y)) + geom_col(fill = "navy") +
      ggtitle(paste("[", sum(y), " responses]")) + 
      geom_text(aes(label = y), vjust = -0.2) +
      ylab("Number of response") + theme(axis.title.x = element_blank(), 
                                         axis.text = element_text(size = 14), 
                                         axis.title = element_text(size = 14, face = "bold"), 
                                         plot.title = element_text(size = 20))
  })
  
  #QUESTION: Would you recommend this session to a colleague
  output$WCwD_recommend_plot <- renderPlot({
    # Create x vector to five survey response options
    # Create x vector to five survey response options
    x = c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")
    
    # Create y vector that counts the number of each response type
    y = c(length(which(WCwD_feedback[9] == "Strongly Disagree")) + length(which(WCwD_feedback[9] == "Strongly disagree")),
          length(which(WCwD_feedback[9] == "Disagree")),
          length(which(WCwD_feedback[9] == "Neutral")),
          length(which(WCwD_feedback[9] == "Agree")),
          length(which(WCwD_feedback[9] == "Strongly Agree")) + length(which(WCwD_feedback[9] == "Strongly agree")))
    
    # Create data frame
    df <- data.frame(x, y)
    
    # Column plot results
    ggplot(df, aes(x = fct_inorder(x, ordered = NA), y = y)) + geom_col(fill = "navy") + 
      ggtitle(paste("[", sum(y), " responses]")) + 
      geom_text(aes(label = y), vjust = -0.2) +
      ylab("Number of response") + theme(axis.title.x = element_blank(), 
                                         axis.text = element_text(size = 14), 
                                         axis.title = element_text(size = 14, face = "bold"), 
                                         plot.title = element_text(size = 20))
  })
  
  
  # Summary Statistics----------------------------------------------------------
  # Attendees as a proportion of those who registered                                        
  WCwD_prop_attend <- function(){
    round((as.numeric(WCwD_summary_statistics[4]) / 
             as.numeric(WCwD_summary_statistics[2])) * 100, digits = 0)}
  output$WCwD_prop_attend_box <- renderValueBox({
    valueBox(paste0(WCwD_prop_attend(), "%"), 
             "Attendees as a proportion of those who registered", color = RAG(WCwD_prop_attend() / 100))})
  
  # Number of attendees
  WCwD_num_attendees <- function(){WCwD_summary_statistics[4]}
  output$WCwD_num_attendees_box <- renderValueBox({
    valueBox(paste0(WCwD_num_attendees()), "Number of attendees", color = "light-blue")})
  
  # Number of registrations
  WCwD_num_registrations <- function(){WCwD_summary_statistics[2]}
  output$WCwD_num_registrations_box <- renderValueBox({
    valueBox(paste0(WCwD_num_registrations()), "Number of registrations", color = "light-blue")})
  
  # Proportion of people who viewed registration page that then signed up, (num registered + num cancelled) / num views
  WCwD_prop_register <- function(){
    round(((as.numeric(WCwD_summary_statistics[2]) + 
              as.numeric(WCwD_summary_statistics[3])) / 
             as.numeric(WCwD_summary_statistics[1])) * 100, digits = 0)}
  output$WCwD_prop_register_box <- renderValueBox({
    valueBox(paste0(WCwD_prop_register(), "%"), 
             "Proportion of people who viewed registration page that then signed up", 
             color = RAG(WCwD_prop_register() / 100))}) 
  
  # Number of page views
  WCwD_num_page_views <- function(){WCwD_summary_statistics[1]}
  output$WCwD_num_page_views_box <- renderValueBox({
    valueBox(paste0(WCwD_num_page_views()), "Number of page views", color = "light-blue")})
  
  # Number of cancellations
  WCwD_num_cancel <- function(){as.numeric(WCwD_summary_statistics[3])}
  output$WCwD_num_cancel_box <- renderValueBox({
    valueBox(paste0(WCwD_num_cancel()), "Number of cancellations", color = "light-blue")})
  
  # Average attendance time
  WCwD_avg_attend_time <- function(){WCwD_summary_statistics[6]}
  output$WCwD_avg_attend_time_box <- renderValueBox({
    valueBox(paste0(WCwD_avg_attend_time()), "Average attendance duration", color = "light-blue")})
  
  # Training duration
  WCwD_train_duration <- function(){"1h 00m 00s"}
  output$WCwD_train_duration_box <- renderValueBox({
    valueBox(paste0(WCwD_train_duration()), "Target training duration", color = "light-blue")})
  
  # Meeting duration
  WCwD_meet_duration <- function(){WCwD_summary_statistics[5]}
  output$WCwD_meet_duration_box <- renderValueBox({
    valueBox(paste0(WCwD_meet_duration()), "Actual training duration", color = "light-blue")})
  
  
  
  
  # INTRODUCTION TO DATA VISUALISATIONS : PART 1================================
  # Attendee Characteristics----------------------------------------------------
  # Grade analysis 
  output$DV1_grade <- renderPlot({
    x = c("AO/AA", "EO", "HEO", "SEO", "Grade 7", "Grade 6", "SCS")
    
    y = c(length(which(DV1_attendee_characteristics[2] == "AO/AA")),
          length(which(DV1_attendee_characteristics[2] == "EO")),
          length(which(DV1_attendee_characteristics[2] == "HEO")),
          length(which(DV1_attendee_characteristics[2] == "SEO")),
          length(which(DV1_attendee_characteristics[2] == "Grade 7")),
          length(which(DV1_attendee_characteristics[2] == "Grade 6")),
          length(which(DV1_attendee_characteristics[2] == "SCS")))
    
    # Create data frame
    df <- data.frame(x, y)
    
    # Column plot results
    ggplot(df, aes(x = fct_inorder(x, ordered = NA), y = y)) + geom_col(fill = "navy") + 
      xlab("Grade") + ylab("Number in attendance") + theme(axis.text = element_text(size = 14), 
                                                           axis.title = element_text(size = 14, face = "bold"), 
                                                           plot.title = element_text(size = 20))
  })
  
  # DG group analysis
  output$DV1_DG_group <- renderPlot({
    x = c("Environment", "Food, Biosecurity, and Trade", "Portfolio Delivery", 
          "Strategy", "Science and Analysis")
    
    y = c(length(which(DV1_attendee_characteristics[3] == "Environment")),
          length(which(DV1_attendee_characteristics[3] == "Food, Biosecurity, and Trade")),
          length(which(DV1_attendee_characteristics[3] == "Portfolio Delivery")),
          length(which(DV1_attendee_characteristics[3] == "Strategy")),
          length(which(DV1_attendee_characteristics[3] == "Science and Analysis")))
    
    # Create data frame
    df <- data.frame(x, y)
    
    # Column plot results
    ggplot(df, aes(x = reorder(fct_inorder(x, ordered = NA), y), y = y)) +
      geom_bar(stat = "identity", fill = "navy") + coord_flip() + xlab("DG Group") +
      ylab("Number in attendance") + theme(axis.text = element_text(size = 14), 
                                           axis.title = element_text(size = 14, face = "bold"), 
                                           plot.title = element_text(size = 20))
  })
  
  # Profession analysis
  output$DV1_profession <- renderPlot({
    x = c("Operational Delivery",
          "Policy",
          "Commercial",
          "Communications",
          "Digital and Data",
          "Economics",
          "Finance",
          "Geography",
          "Human Resources",
          "Project Delivery",
          "Property",
          "Operational Research",
          "Social Research",
          "Statistics",
          "Legal",
          "Science and Engineering",
          "Veterinary",
          "Other")
    
    y = c(length(which(DV1_attendee_characteristics[1] == "Operational Delivery")),
          length(which(DV1_attendee_characteristics[1] == "Policy")),
          length(which(DV1_attendee_characteristics[1] == "Commercial")),
          length(which(DV1_attendee_characteristics[1] == "Communications")),
          length(which(DV1_attendee_characteristics[1] == "Digital and Data")),
          length(which(DV1_attendee_characteristics[1] == "Economics")),
          length(which(DV1_attendee_characteristics[1] == "Finance")),
          length(which(DV1_attendee_characteristics[1] == "Geography")),
          length(which(DV1_attendee_characteristics[1] == "Human Resources")),
          length(which(DV1_attendee_characteristics[1] == "Project Delivery")),
          length(which(DV1_attendee_characteristics[1] == "Property")),
          length(which(DV1_attendee_characteristics[1] == "Operational Research")),
          length(which(DV1_attendee_characteristics[1] == "Social Research")),
          length(which(DV1_attendee_characteristics[1] == "Statistics")),
          length(which(DV1_attendee_characteristics[1] == "Legal")),
          length(which(DV1_attendee_characteristics[1] == "Science and Engineering")),
          length(which(DV1_attendee_characteristics[1] == "Veterinary")),
          length(which(DV1_attendee_characteristics[1] == "Other")))
    
    # Create data frame
    df <- data.frame(x, y)
    
    # Column plot results
    ggplot(df, aes(x = reorder(fct_inorder(x, ordered = NA), y), y = y)) + 
      geom_bar(stat = "identity", fill = "navy") + coord_flip() + xlab("Profession") +
      ylab("Number in attendance") + theme(axis.text = element_text(size = 14), 
                                           axis.title = element_text(size = 14, face = "bold"), 
                                           plot.title = element_text(size = 20))
  })
  
  # ALB Analysis
  output$DV1_ALB <- renderPlot({
    x = c("Animal and Plant Health Agency", 
          "Centre for Environment, Fisheries, and Aquaculture Science",
          "Environment Agency", "Forestry Commission", "Marine Management Organisation", 
          "Natural England", "Rural Payments Agency", "Veterinary Medicines Directorate",
          "Other", "N/A")
    
    y = c(length(which(DV1_attendee_characteristics[4] == "Animal and Plant Health Agenc")),
          length(which(DV1_attendee_characteristics[4] == "Centre for Environment, Fisheries, and Aquaculture Science")),
          length(which(DV1_attendee_characteristics[4] == "Environment Agency")),
          length(which(DV1_attendee_characteristics[4] == "Forestry Commission")),
          length(which(DV1_attendee_characteristics[4] == "Marine Management Organisation")),
          length(which(DV1_attendee_characteristics[4] == "Natural England")),
          length(which(DV1_attendee_characteristics[4] == "Rural Payments Agency")),
          length(which(DV1_attendee_characteristics[4] == "Veterinary Medicines Directorate")),
          length(which(DV1_attendee_characteristics[4] == "Other")),
          length(which(DV1_attendee_characteristics[4] == "N/A")))
    
    # Create data frame
    df <- data.frame(x, y)
    
    # Column plot results
    ggplot(df, aes(x = reorder(fct_inorder(x, ordered = NA), y), y = y)) + geom_bar(stat = "identity", fill = "navy") + coord_flip() + 
      xlab("ALB") + ylab("Number in attendance") + theme(axis.text = element_text(size = 14), 
                                                         axis.title = element_text(size = 14, face = "bold"), 
                                                         plot.title = element_text(size = 20))
  })
  
  
  # Feedback polls--------------------------------------------------------------
  # Number of responses = number of rows
  DV1_num_responses <- function(){nrow(DV1_feedback)}
  output$DV1_num_responses <- renderValueBox({
    valueBox(paste0(DV1_num_responses()), "Number of responses", color = "olive")})
  
  # Feedback response rate 
  DV1_feedback_response_rate <- function(){
    round(((nrow(DV1_feedback)) / as.numeric(DV1_summary_statistics[4])) * 100, digits = 0)}
  output$DV1_feedback_response_rate <- renderValueBox({
    valueBox(paste0(DV1_feedback_response_rate(), "%"), "Feedback response rate", color = RAG(DV1_feedback_response_rate() / 100))})
  
  # QUESTION: Did you find this session useful?
  output$DV1_useful_plot <- renderPlot({
    # Create x vector to five survey response options
    x = c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")
    
    # Create y vector that counts the number of each response type
    y = c(length(which(DV1_feedback[6] == "Strongly Disagree")) + length(which(DV1_feedback[6] == "Strongly disagree")),
          length(which(DV1_feedback[6] == "Disagree")),
          length(which(DV1_feedback[6] == "Neutral")),
          length(which(DV1_feedback[6] == "Agree")),
          length(which(DV1_feedback[6] == "Strongly Agree")) + length(which(DV1_feedback[6] == "Strongly agree")))
    
    # Create data frame
    df <- data.frame(x, y)
    
    # Column plot results
    ggplot(df, aes(x = fct_inorder(x, ordered = NA), y = y)) + geom_col(fill = "navy") +
      ggtitle(paste("[", sum(y), " responses]")) +
      geom_text(aes(label = y), vjust = -0.2) +
      ylab("Number of response") + theme(axis.title.x = element_blank(), 
                                         axis.text = element_text(size = 14), 
                                         axis.title = element_text(size = 14, face = "bold"), 
                                         plot.title = element_text(size = 20)) 
  })
  
  #QUESTION: Did you find this session engaging?
  output$DV1_engaging_plot <- renderPlot({
    # Create x vector to five survey response options
    x = c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")
    
    # Create y vector that counts the number of each response type
    y = c(length(which(DV1_feedback[7] == "Strongly Disagree")) + length(which(DV1_feedback[7] == "Strongly disagree")),
          length(which(DV1_feedback[7] == "Disagree")),
          length(which(DV1_feedback[7] == "Neutral")),
          length(which(DV1_feedback[7] == "Agree")),
          length(which(DV1_feedback[7] == "Strongly Agree")) + length(which(DV1_feedback[7] == "Strongly agree")))
    
    # Create data frame
    df <- data.frame(x, y)
    
    # Column plot results
    ggplot(df, aes(x = fct_inorder(x, ordered = NA), y = y)) + geom_col(fill = "navy") + 
      ggtitle(paste("[", sum(y), " responses]")) +
      geom_text(aes(label = y), vjust = -0.2) +
      ylab("Number of response") + theme(axis.title.x = element_blank(), 
                                         axis.text = element_text(size = 14), 
                                         axis.title = element_text(size = 14, face = "bold"), 
                                         plot.title = element_text(size = 20))
  })
  
  #QUESTION: Did you learn something from this session?
  output$DV1_learning_plot <- renderPlot({
    # Create x vector to five survey response options
    x = c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")
    
    # Create y vector that counts the number of each response type
    y = c(length(which(DV1_feedback[8] == "Strongly Disagree")) + length(which(DV1_feedback[8] == "Strongly disagree")),
          length(which(DV1_feedback[8] == "Disagree")),
          length(which(DV1_feedback[8] == "Neutral")),
          length(which(DV1_feedback[8] == "Agree")),
          length(which(DV1_feedback[8] == "Strongly Agree")) + length(which(DV1_feedback[8] == "Strongly agree")))
    
    # Create data frame
    df <- data.frame(x, y)
    
    # Column plot results
    ggplot(df, aes(x = fct_inorder(x, ordered = NA), y = y)) + geom_col(fill = "navy") +
      ggtitle(paste("[", sum(y), " responses]")) + 
      geom_text(aes(label = y), vjust = -0.2) +
      ylab("Number of response") + theme(axis.title.x = element_blank(), 
                                         axis.text = element_text(size = 14), 
                                         axis.title = element_text(size = 14, face = "bold"), 
                                         plot.title = element_text(size = 20))
  })
  
  #QUESTION: Would you recommend this session to a colleague
  output$DV1_recommend_plot <- renderPlot({
    # Create x vector to five survey response options
    # Create x vector to five survey response options
    x = c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")
    
    # Create y vector that counts the number of each response type
    y = c(length(which(DV1_feedback[9] == "Strongly Disagree")) + length(which(DV1_feedback[9] == "Strongly disagree")),
          length(which(DV1_feedback[9] == "Disagree")),
          length(which(DV1_feedback[9] == "Neutral")),
          length(which(DV1_feedback[9] == "Agree")),
          length(which(DV1_feedback[9] == "Strongly Agree")) + length(which(DV1_feedback[9] == "Strongly agree")))
    
    # Create data frame
    df <- data.frame(x, y)
    
    # Column plot results
    ggplot(df, aes(x = fct_inorder(x, ordered = NA), y = y)) + geom_col(fill = "navy") + 
      ggtitle(paste("[", sum(y), " responses]")) + 
      geom_text(aes(label = y), vjust = -0.2) +
      ylab("Number of response") + theme(axis.title.x = element_blank(), 
                                         axis.text = element_text(size = 14), 
                                         axis.title = element_text(size = 14, face = "bold"), 
                                         plot.title = element_text(size = 20))
  })
  
  
  # Summary Statistics----------------------------------------------------------
  # Attendees as a proportion of those who registered                                        
  DV1_prop_attend <- function(){
    round((as.numeric(DV1_summary_statistics[4]) / 
             as.numeric(DV1_summary_statistics[2])) * 100, digits = 0)}
  output$DV1_prop_attend_box <- renderValueBox({
    valueBox(paste0(DV1_prop_attend(), "%"), 
             "Attendees as a proportion of those who registered", color = RAG(DV1_prop_attend() / 100))})
  
  # Number of attendees
  DV1_num_attendees <- function(){DV1_summary_statistics[4]}
  output$DV1_num_attendees_box <- renderValueBox({
    valueBox(paste0(DV1_num_attendees()), "Number of attendees", color = "light-blue")})
  
  # Number of registrations
  DV1_num_registrations <- function(){DV1_summary_statistics[2]}
  output$DV1_num_registrations_box <- renderValueBox({
    valueBox(paste0(DV1_num_registrations()), "Number of registrations", color = "light-blue")})
  
  # Proportion of people who viewed registration page that then signed up, (num registered + num cancelled) / num views
  DV1_prop_register <- function(){
    round(((as.numeric(DV1_summary_statistics[2]) + 
              as.numeric(DV1_summary_statistics[3])) / 
             as.numeric(DV1_summary_statistics[1])) * 100, digits = 0)}
  output$DV1_prop_register_box <- renderValueBox({
    valueBox(paste0(DV1_prop_register(), "%"), 
             "Proportion of people who viewed registration page that then signed up", 
             color = RAG(DV1_prop_register() / 100))}) 
  
  # Number of page views
  DV1_num_page_views <- function(){DV1_summary_statistics[1]}
  output$DV1_num_page_views_box <- renderValueBox({
    valueBox(paste0(DV1_num_page_views()), "Number of page views", color = "light-blue")})
  
  # Number of cancellations
  DV1_num_cancel <- function(){as.numeric(DV1_summary_statistics[3])}
  output$DV1_num_cancel_box <- renderValueBox({
    valueBox(paste0(DV1_num_cancel()), "Number of cancellations", color = "light-blue")})
  
  # Average attendance time
  DV1_avg_attend_time <- function(){DV1_summary_statistics[6]}
  output$DV1_avg_attend_time_box <- renderValueBox({
    valueBox(paste0(DV1_avg_attend_time()), "Average attendance duration", color = "light-blue")})
  
  # Training duration
  DV1_train_duration <- function(){"1h 00m 00s"}
  output$DV1_train_duration_box <- renderValueBox({
    valueBox(paste0(DV1_train_duration()), "Target training duration", color = "light-blue")})
  
  # Meeting duration
  DV1_meet_duration <- function(){DV1_summary_statistics[5]}
  output$DV1_meet_duration_box <- renderValueBox({
    valueBox(paste0(DV1_meet_duration()), "Actual training duration", color = "light-blue")})
  
  
  
  # INTRODUCTION TO DATA VISUALISATIONS : PART 2================================
  # Attendee Characteristics----------------------------------------------------
  # Grade analysis 
  output$DV2_grade <- renderPlot({
    x = c("AO/AA", "EO", "HEO", "SEO", "Grade 7", "Grade 6", "SCS")
    
    y = c(length(which(DV2_attendee_characteristics[2] == "AO/AA")),
          length(which(DV2_attendee_characteristics[2] == "EO")),
          length(which(DV2_attendee_characteristics[2] == "HEO")),
          length(which(DV2_attendee_characteristics[2] == "SEO")),
          length(which(DV2_attendee_characteristics[2] == "Grade 7")),
          length(which(DV2_attendee_characteristics[2] == "Grade 6")),
          length(which(DV2_attendee_characteristics[2] == "SCS")))
    
    # Create data frame
    df <- data.frame(x, y)
    
    # Column plot results
    ggplot(df, aes(x = fct_inorder(x, ordered = NA), y = y)) + geom_col(fill = "navy") + 
      xlab("Grade") + ylab("Number in attendance") + theme(axis.text = element_text(size = 14), 
                                                           axis.title = element_text(size = 14, face = "bold"), 
                                                           plot.title = element_text(size = 20))
  })
  
  # DG group analysis
  output$DV2_DG_group <- renderPlot({
    x = c("Environment", "Food, Biosecurity, and Trade", "Portfolio Delivery", 
          "Strategy", "Science and Analysis")
    
    y = c(length(which(DV2_attendee_characteristics[3] == "Environment")),
          length(which(DV2_attendee_characteristics[3] == "Food, Biosecurity, and Trade")),
          length(which(DV2_attendee_characteristics[3] == "Portfolio Delivery")),
          length(which(DV2_attendee_characteristics[3] == "Strategy")),
          length(which(DV2_attendee_characteristics[3] == "Science and Analysis")))
    
    # Create data frame
    df <- data.frame(x, y)
    
    # Column plot results
    ggplot(df, aes(x = reorder(fct_inorder(x, ordered = NA), y), y = y)) +
      geom_bar(stat = "identity", fill = "navy") + coord_flip() + xlab("DG Group") +
      ylab("Number in attendance") + theme(axis.text = element_text(size = 14), 
                                           axis.title = element_text(size = 14, face = "bold"), 
                                           plot.title = element_text(size = 20))
  })
  
  # Profession analysis
  output$DV2_profession <- renderPlot({
    x = c("Operational Delivery",
          "Policy",
          "Commercial",
          "Communications",
          "Digital and Data",
          "Economics",
          "Finance",
          "Geography",
          "Human Resources",
          "Project Delivery",
          "Property",
          "Operational Research",
          "Social Research",
          "Statistics",
          "Legal",
          "Science and Engineering",
          "Veterinary",
          "Other")
    
    y = c(length(which(DV2_attendee_characteristics[1] == "Operational Delivery")),
          length(which(DV2_attendee_characteristics[1] == "Policy")),
          length(which(DV2_attendee_characteristics[1] == "Commercial")),
          length(which(DV2_attendee_characteristics[1] == "Communications")),
          length(which(DV2_attendee_characteristics[1] == "Digital and Data")),
          length(which(DV2_attendee_characteristics[1] == "Economics")),
          length(which(DV2_attendee_characteristics[1] == "Finance")),
          length(which(DV2_attendee_characteristics[1] == "Geography")),
          length(which(DV2_attendee_characteristics[1] == "Human Resources")),
          length(which(DV2_attendee_characteristics[1] == "Project Delivery")),
          length(which(DV2_attendee_characteristics[1] == "Property")),
          length(which(DV2_attendee_characteristics[1] == "Operational Research")),
          length(which(DV2_attendee_characteristics[1] == "Social Research")),
          length(which(DV2_attendee_characteristics[1] == "Statistics")),
          length(which(DV2_attendee_characteristics[1] == "Legal")),
          length(which(DV2_attendee_characteristics[1] == "Science and Engineering")),
          length(which(DV2_attendee_characteristics[1] == "Veterinary")),
          length(which(DV2_attendee_characteristics[1] == "Other")))
    
    # Create data frame
    df <- data.frame(x, y)
    
    # Column plot results
    ggplot(df, aes(x = reorder(fct_inorder(x, ordered = NA), y), y = y)) + 
      geom_bar(stat = "identity", fill = "navy") + coord_flip() + xlab("Profession") +
      ylab("Number in attendance") + theme(axis.text = element_text(size = 14), 
                                           axis.title = element_text(size = 14, face = "bold"), 
                                           plot.title = element_text(size = 20))
  })
  
  # ALB Analysis
  output$DV2_ALB <- renderPlot({
    x = c("Animal and Plant Health Agency", 
          "Centre for Environment, Fisheries, and Aquaculture Science",
          "Environment Agency", "Forestry Commission", "Marine Management Organisation", 
          "Natural England", "Rural Payments Agency", "Veterinary Medicines Directorate",
          "Other", "N/A")
    
    y = c(length(which(DV2_attendee_characteristics[4] == "Animal and Plant Health Agenc")),
          length(which(DV2_attendee_characteristics[4] == "Centre for Environment, Fisheries, and Aquaculture Science")),
          length(which(DV2_attendee_characteristics[4] == "Environment Agency")),
          length(which(DV2_attendee_characteristics[4] == "Forestry Commission")),
          length(which(DV2_attendee_characteristics[4] == "Marine Management Organisation")),
          length(which(DV2_attendee_characteristics[4] == "Natural England")),
          length(which(DV2_attendee_characteristics[4] == "Rural Payments Agency")),
          length(which(DV2_attendee_characteristics[4] == "Veterinary Medicines Directorate")),
          length(which(DV2_attendee_characteristics[4] == "Other")),
          length(which(DV2_attendee_characteristics[4] == "N/A")))
    
    # Create data frame
    df <- data.frame(x, y)
    
    # Column plot results
    ggplot(df, aes(x = reorder(fct_inorder(x, ordered = NA), y), y = y)) + geom_bar(stat = "identity", fill = "navy") + coord_flip() + 
      xlab("ALB") + ylab("Number in attendance") + theme(axis.text = element_text(size = 14), 
                                                         axis.title = element_text(size = 14, face = "bold"), 
                                                         plot.title = element_text(size = 20))
  })
  
  
  # Feedback polls--------------------------------------------------------------
  # Number of responses = number of rows
  DV2_num_responses <- function(){nrow(DV2_feedback)}
  output$DV2_num_responses <- renderValueBox({
    valueBox(paste0(DV2_num_responses()), "Number of responses", color = "olive")})
  
  # Feedback response rate 
  DV2_feedback_response_rate <- function(){
    round(((nrow(DV2_feedback)) / as.numeric(DV2_summary_statistics[4])) * 100, digits = 0)}
  output$DV2_feedback_response_rate <- renderValueBox({
    valueBox(paste0(DV2_feedback_response_rate(), "%"), "Feedback response rate", color = RAG(DV2_feedback_response_rate() / 100))})
  
  # QUESTION: Did you find this session useful?
  output$DV2_useful_plot <- renderPlot({
    # Create x vector to five survey response options
    x = c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")
    
    # Create y vector that counts the number of each response type
    y = c(length(which(DV2_feedback[6] == "Strongly Disagree")) + length(which(DV2_feedback[6] == "Strongly disagree")),
          length(which(DV2_feedback[6] == "Disagree")),
          length(which(DV2_feedback[6] == "Neutral")),
          length(which(DV2_feedback[6] == "Agree")),
          length(which(DV2_feedback[6] == "Strongly Agree")) + length(which(DV2_feedback[6] == "Strongly agree")))
    
    # Create data frame
    df <- data.frame(x, y)
    
    # Column plot results
    ggplot(df, aes(x = fct_inorder(x, ordered = NA), y = y)) + geom_col(fill = "navy") +
      ggtitle(paste("[", sum(y), " responses]")) +
      geom_text(aes(label = y), vjust = -0.2) +
      ylab("Number of response") + theme(axis.title.x = element_blank(), 
                                         axis.text = element_text(size = 14), 
                                         axis.title = element_text(size = 14, face = "bold"), 
                                         plot.title = element_text(size = 20)) 
  })
  
  #QUESTION: Did you find this session engaging?
  output$DV2_engaging_plot <- renderPlot({
    # Create x vector to five survey response options
    x = c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")
    
    # Create y vector that counts the number of each response type
    y = c(length(which(DV2_feedback[7] == "Strongly Disagree")) + length(which(DV2_feedback[7] == "Strongly disagree")),
          length(which(DV2_feedback[7] == "Disagree")),
          length(which(DV2_feedback[7] == "Neutral")),
          length(which(DV2_feedback[7] == "Agree")),
          length(which(DV2_feedback[7] == "Strongly Agree")) + length(which(DV2_feedback[7] == "Strongly agree")))
    
    # Create data frame
    df <- data.frame(x, y)
    
    # Column plot results
    ggplot(df, aes(x = fct_inorder(x, ordered = NA), y = y)) + geom_col(fill = "navy") + 
      ggtitle(paste("[", sum(y), " responses]")) +
      geom_text(aes(label = y), vjust = -0.2) +
      ylab("Number of response") + theme(axis.title.x = element_blank(), 
                                         axis.text = element_text(size = 14), 
                                         axis.title = element_text(size = 14, face = "bold"), 
                                         plot.title = element_text(size = 20))
  })
  
  #QUESTION: Did you learn something from this session?
  output$DV2_learning_plot <- renderPlot({
    # Create x vector to five survey response options
    x = c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")
    
    # Create y vector that counts the number of each response type
    y = c(length(which(DV2_feedback[8] == "Strongly Disagree")) + length(which(DV2_feedback[8] == "Strongly disagree")),
          length(which(DV2_feedback[8] == "Disagree")),
          length(which(DV2_feedback[8] == "Neutral")),
          length(which(DV2_feedback[8] == "Agree")),
          length(which(DV2_feedback[8] == "Strongly Agree")) + length(which(DV2_feedback[8] == "Strongly agree")))
    
    # Create data frame
    df <- data.frame(x, y)
    
    # Column plot results
    ggplot(df, aes(x = fct_inorder(x, ordered = NA), y = y)) + geom_col(fill = "navy") +
      ggtitle(paste("[", sum(y), " responses]")) + 
      geom_text(aes(label = y), vjust = -0.2) +
      ylab("Number of response") + theme(axis.title.x = element_blank(), 
                                         axis.text = element_text(size = 14), 
                                         axis.title = element_text(size = 14, face = "bold"), 
                                         plot.title = element_text(size = 20))
  })
  
  #QUESTION: Would you recommend this session to a colleague
  output$DV2_recommend_plot <- renderPlot({
    # Create x vector to five survey response options
    # Create x vector to five survey response options
    x = c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")
    
    # Create y vector that counts the number of each response type
    y = c(length(which(DV2_feedback[9] == "Strongly Disagree")) + length(which(DV2_feedback[9] == "Strongly disagree")),
          length(which(DV2_feedback[9] == "Disagree")),
          length(which(DV2_feedback[9] == "Neutral")),
          length(which(DV2_feedback[9] == "Agree")),
          length(which(DV2_feedback[9] == "Strongly Agree")) + length(which(DV2_feedback[9] == "Strongly agree")))
    
    # Create data frame
    df <- data.frame(x, y)
    
    # Column plot results
    ggplot(df, aes(x = fct_inorder(x, ordered = NA), y = y)) + geom_col(fill = "navy") + 
      ggtitle(paste("[", sum(y), " responses]")) + 
      geom_text(aes(label = y), vjust = -0.2) +
      ylab("Number of response") + theme(axis.title.x = element_blank(), 
                                         axis.text = element_text(size = 14), 
                                         axis.title = element_text(size = 14, face = "bold"), 
                                         plot.title = element_text(size = 20))
  })
  
  # Summary Statistics----------------------------------------------------------
  # Attendees as a proportion of those who registered                                        
  DV2_prop_attend <- function(){
    round((as.numeric(DV2_summary_statistics[4]) / 
             as.numeric(DV2_summary_statistics[2])) * 100, digits = 0)}
  output$DV2_prop_attend_box <- renderValueBox({
    valueBox(paste0(DV2_prop_attend(), "%"), 
             "Attendees as a proportion of those who registered", color = RAG(DV2_prop_attend() / 100))})
  
  # Number of attendees
  DV2_num_attendees <- function(){DV2_summary_statistics[4]}
  output$DV2_num_attendees_box <- renderValueBox({
    valueBox(paste0(DV2_num_attendees()), "Number of attendees", color = "light-blue")})
  
  # Number of registrations
  DV2_num_registrations <- function(){DV2_summary_statistics[2]}
  output$DV2_num_registrations_box <- renderValueBox({
    valueBox(paste0(DV2_num_registrations()), "Number of registrations", color = "light-blue")})
  
  # Proportion of people who viewed registration page that then signed up, (num registered + num cancelled) / num views
  DV2_prop_register <- function(){
    round(((as.numeric(DV2_summary_statistics[2]) + 
              as.numeric(DV2_summary_statistics[3])) / 
             as.numeric(DV2_summary_statistics[1])) * 100, digits = 0)}
  output$DV2_prop_register_box <- renderValueBox({
    valueBox(paste0(DV2_prop_register(), "%"), 
             "Proportion of people who viewed registration page that then signed up", 
             color = RAG(DV2_prop_register() / 100))}) 
  
  # Number of page views
  DV2_num_page_views <- function(){DV2_summary_statistics[1]}
  output$DV2_num_page_views_box <- renderValueBox({
    valueBox(paste0(DV2_num_page_views()), "Number of page views", color = "light-blue")})
  
  # Number of cancellations
  DV2_num_cancel <- function(){as.numeric(DV2_summary_statistics[3])}
  output$DV2_num_cancel_box <- renderValueBox({
    valueBox(paste0(DV2_num_cancel()), "Number of cancellations", color = "light-blue")})
  
  # Average attendance time
  DV2_avg_attend_time <- function(){DV2_summary_statistics[6]}
  output$DV2_avg_attend_time_box <- renderValueBox({
    valueBox(paste0(DV2_avg_attend_time()), "Average attendance duration", color = "light-blue")})
  
  # Training duration
  DV2_train_duration <- function(){"1h 00m 00s"}
  output$DV2_train_duration_box <- renderValueBox({
    valueBox(paste0(DV2_train_duration()), "Target training duration", color = "light-blue")})
  
  # Meeting duration
  DV2_meet_duration <- function(){DV2_summary_statistics[5]}
  output$DV2_meet_duration_box <- renderValueBox({
    valueBox(paste0(DV2_meet_duration()), "Actual training duration", color = "light-blue")})
  
  
  
  
  # QUIZ! UNDERSTANDING ANALYTICAL GUIDANCE=====================================
  # Attendee Characteristics----------------------------------------------------
  # Grade analysis 
  output$QUIZ_grade <- renderPlot({
    x = c("AO/AA", "EO", "HEO", "SEO", "Grade 7", "Grade 6", "SCS")
    
    y = c(length(which(QUIZ_attendee_characteristics[2] == "AO/AA")),
          length(which(QUIZ_attendee_characteristics[2] == "EO")),
          length(which(QUIZ_attendee_characteristics[2] == "HEO")),
          length(which(QUIZ_attendee_characteristics[2] == "SEO")),
          length(which(QUIZ_attendee_characteristics[2] == "Grade 7")),
          length(which(QUIZ_attendee_characteristics[2] == "Grade 6")),
          length(which(QUIZ_attendee_characteristics[2] == "SCS")))
    
    # Create data frame
    df <- data.frame(x, y)
    
    # Column plot results
    ggplot(df, aes(x = fct_inorder(x, ordered = NA), y = y)) + geom_col(fill = "navy") + 
      xlab("Grade") + ylab("Number in attendance") + theme(axis.text = element_text(size = 14), 
                                                           axis.title = element_text(size = 14, face = "bold"), 
                                                           plot.title = element_text(size = 20))
  })
  
  # DG group analysis
  output$QUIZ_DG_group <- renderPlot({
    x = c("Environment", "Food, Biosecurity, and Trade", "Portfolio Delivery", 
          "Strategy", "Science and Analysis")
    
    y = c(length(which(QUIZ_attendee_characteristics[3] == "Environment")),
          length(which(QUIZ_attendee_characteristics[3] == "Food, Biosecurity, and Trade")),
          length(which(QUIZ_attendee_characteristics[3] == "Portfolio Delivery")),
          length(which(QUIZ_attendee_characteristics[3] == "Strategy")),
          length(which(QUIZ_attendee_characteristics[3] == "Science and Analysis")))
    
    # Create data frame
    df <- data.frame(x, y)
    
    # Column plot results
    ggplot(df, aes(x = reorder(fct_inorder(x, ordered = NA), y), y = y)) +
      geom_bar(stat = "identity", fill = "navy") + coord_flip() + xlab("DG Group") +
      ylab("Number in attendance") + theme(axis.text = element_text(size = 14), 
                                           axis.title = element_text(size = 14, face = "bold"), 
                                           plot.title = element_text(size = 20))
  })
  
  # Profession analysis
  output$QUIZ_profession <- renderPlot({
    x = c("Operational Delivery",
          "Policy",
          "Commercial",
          "Communications",
          "Digital and Data",
          "Economics",
          "Finance",
          "Geography",
          "Human Resources",
          "Project Delivery",
          "Property",
          "Operational Research",
          "Social Research",
          "Statistics",
          "Legal",
          "Science and Engineering",
          "Veterinary",
          "Other")
    
    y = c(length(which(QUIZ_attendee_characteristics[1] == "Operational Delivery")),
          length(which(QUIZ_attendee_characteristics[1] == "Policy")),
          length(which(QUIZ_attendee_characteristics[1] == "Commercial")),
          length(which(QUIZ_attendee_characteristics[1] == "Communications")),
          length(which(QUIZ_attendee_characteristics[1] == "Digital and Data")),
          length(which(QUIZ_attendee_characteristics[1] == "Economics")),
          length(which(QUIZ_attendee_characteristics[1] == "Finance")),
          length(which(QUIZ_attendee_characteristics[1] == "Geography")),
          length(which(QUIZ_attendee_characteristics[1] == "Human Resources")),
          length(which(QUIZ_attendee_characteristics[1] == "Project Delivery")),
          length(which(QUIZ_attendee_characteristics[1] == "Property")),
          length(which(QUIZ_attendee_characteristics[1] == "Operational Research")),
          length(which(QUIZ_attendee_characteristics[1] == "Social Research")),
          length(which(QUIZ_attendee_characteristics[1] == "Statistics")),
          length(which(QUIZ_attendee_characteristics[1] == "Legal")),
          length(which(QUIZ_attendee_characteristics[1] == "Science and Engineering")),
          length(which(QUIZ_attendee_characteristics[1] == "Veterinary")),
          length(which(QUIZ_attendee_characteristics[1] == "Other")))
    
    # Create data frame
    df <- data.frame(x, y)
    
    # Column plot results
    ggplot(df, aes(x = reorder(fct_inorder(x, ordered = NA), y), y = y)) + 
      geom_bar(stat = "identity", fill = "navy") + coord_flip() + xlab("Profession") +
      ylab("Number in attendance") + theme(axis.text = element_text(size = 14), 
                                           axis.title = element_text(size = 14, face = "bold"), 
                                           plot.title = element_text(size = 20))
  })
  
  # ALB Analysis
  output$QUIZ_ALB <- renderPlot({
    x = c("Animal and Plant Health Agency", 
          "Centre for Environment, Fisheries, and Aquaculture Science",
          "Environment Agency", "Forestry Commission", "Marine Management Organisation", 
          "Natural England", "Rural Payments Agency", "Veterinary Medicines Directorate",
          "Other", "N/A")
    
    y = c(length(which(QUIZ_attendee_characteristics[4] == "Animal and Plant Health Agenc")),
          length(which(QUIZ_attendee_characteristics[4] == "Centre for Environment, Fisheries, and Aquaculture Science")),
          length(which(QUIZ_attendee_characteristics[4] == "Environment Agency")),
          length(which(QUIZ_attendee_characteristics[4] == "Forestry Commission")),
          length(which(QUIZ_attendee_characteristics[4] == "Marine Management Organisation")),
          length(which(QUIZ_attendee_characteristics[4] == "Natural England")),
          length(which(QUIZ_attendee_characteristics[4] == "Rural Payments Agency")),
          length(which(QUIZ_attendee_characteristics[4] == "Veterinary Medicines Directorate")),
          length(which(QUIZ_attendee_characteristics[4] == "Other")),
          length(which(QUIZ_attendee_characteristics[4] == "N/A")))
    
    # Create data frame
    df <- data.frame(x, y)
    
    # Column plot results
    ggplot(df, aes(x = reorder(fct_inorder(x, ordered = NA), y), y = y)) + geom_bar(stat = "identity", fill = "navy") + coord_flip() + 
      xlab("ALB") + ylab("Number in attendance") + theme(axis.text = element_text(size = 14), 
                                                         axis.title = element_text(size = 14, face = "bold"), 
                                                         plot.title = element_text(size = 20))
  })
  
  
  # Feedback polls--------------------------------------------------------------
  # Number of responses = number of rows
  QUIZ_num_responses <- function(){nrow(QUIZ_feedback)}
  output$QUIZ_num_responses <- renderValueBox({
    valueBox(paste0(QUIZ_num_responses()), "Number of responses", color = "olive")})
  
  # Feedback response rate 
  QUIZ_feedback_response_rate <- function(){
    round(((nrow(QUIZ_feedback)) / as.numeric(QUIZ_summary_statistics[4])) * 100, digits = 0)}
  output$QUIZ_feedback_response_rate <- renderValueBox({
    valueBox(paste0(QUIZ_feedback_response_rate(), "%"), "Feedback response rate", color = RAG(QUIZ_feedback_response_rate() / 100))})
  
  # QUESTION: Did you find this session useful?
  output$QUIZ_useful_plot <- renderPlot({
    # Create x vector to five survey response options
    x = c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")
    
    # Create y vector that counts the number of each response type
    y = c(length(which(QUIZ_feedback[6] == "Strongly Disagree")) + length(which(QUIZ_feedback[6] == "Strongly disagree")),
          length(which(QUIZ_feedback[6] == "Disagree")),
          length(which(QUIZ_feedback[6] == "Neutral")),
          length(which(QUIZ_feedback[6] == "Agree")),
          length(which(QUIZ_feedback[6] == "Strongly Agree")) + length(which(QUIZ_feedback[6] == "Strongly agree")))
    
    # Create data frame
    df <- data.frame(x, y)
    
    # Column plot results
    ggplot(df, aes(x = fct_inorder(x, ordered = NA), y = y)) + geom_col(fill = "navy") +
      ggtitle(paste("[", sum(y), " responses]")) +
      geom_text(aes(label = y), vjust = -0.2) +
      ylab("Number of response") + theme(axis.title.x = element_blank(), 
                                         axis.text = element_text(size = 14), 
                                         axis.title = element_text(size = 14, face = "bold"), 
                                         plot.title = element_text(size = 20)) 
  })
  
  #QUESTION: Did you find this session engaging?
  output$QUIZ_engaging_plot <- renderPlot({
    # Create x vector to five survey response options
    x = c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")
    
    # Create y vector that counts the number of each response type
    y = c(length(which(QUIZ_feedback[7] == "Strongly Disagree")) + length(which(QUIZ_feedback[7] == "Strongly disagree")),
          length(which(QUIZ_feedback[7] == "Disagree")),
          length(which(QUIZ_feedback[7] == "Neutral")),
          length(which(QUIZ_feedback[7] == "Agree")),
          length(which(QUIZ_feedback[7] == "Strongly Agree")) + length(which(QUIZ_feedback[7] == "Strongly agree")))
    
    # Create data frame
    df <- data.frame(x, y)
    
    # Column plot results
    ggplot(df, aes(x = fct_inorder(x, ordered = NA), y = y)) + geom_col(fill = "navy") + 
      ggtitle(paste("[", sum(y), " responses]")) +
      geom_text(aes(label = y), vjust = -0.2) +
      ylab("Number of response") + theme(axis.title.x = element_blank(), 
                                         axis.text = element_text(size = 14), 
                                         axis.title = element_text(size = 14, face = "bold"), 
                                         plot.title = element_text(size = 20))
  })
  
  #QUESTION: Did you learn something from this session?
  output$QUIZ_learning_plot <- renderPlot({
    # Create x vector to five survey response options
    x = c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")
    
    # Create y vector that counts the number of each response type
    y = c(length(which(QUIZ_feedback[8] == "Strongly Disagree")) + length(which(QUIZ_feedback[8] == "Strongly disagree")),
          length(which(QUIZ_feedback[8] == "Disagree")),
          length(which(QUIZ_feedback[8] == "Neutral")),
          length(which(QUIZ_feedback[8] == "Agree")),
          length(which(QUIZ_feedback[8] == "Strongly Agree")) + length(which(QUIZ_feedback[8] == "Strongly agree")))
    
    # Create data frame
    df <- data.frame(x, y)
    
    # Column plot results
    ggplot(df, aes(x = fct_inorder(x, ordered = NA), y = y)) + geom_col(fill = "navy") +
      ggtitle(paste("[", sum(y), " responses]")) + 
      geom_text(aes(label = y), vjust = -0.2) +
      ylab("Number of response") + theme(axis.title.x = element_blank(), 
                                         axis.text = element_text(size = 14), 
                                         axis.title = element_text(size = 14, face = "bold"), 
                                         plot.title = element_text(size = 20))
  })
  
  #QUESTION: Would you recommend this session to a colleague
  output$QUIZ_recommend_plot <- renderPlot({
    # Create x vector to five survey response options
    # Create x vector to five survey response options
    x = c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")
    
    # Create y vector that counts the number of each response type
    y = c(length(which(QUIZ_feedback[9] == "Strongly Disagree")) + length(which(QUIZ_feedback[9] == "Strongly disagree")),
          length(which(QUIZ_feedback[9] == "Disagree")),
          length(which(QUIZ_feedback[9] == "Neutral")),
          length(which(QUIZ_feedback[9] == "Agree")),
          length(which(QUIZ_feedback[9] == "Strongly Agree")) + length(which(QUIZ_feedback[9] == "Strongly agree")))
    
    # Create data frame
    df <- data.frame(x, y)
    
    # Column plot results
    ggplot(df, aes(x = fct_inorder(x, ordered = NA), y = y)) + geom_col(fill = "navy") + 
      ggtitle(paste("[", sum(y), " responses]")) + 
      geom_text(aes(label = y), vjust = -0.2) +
      ylab("Number of response") + theme(axis.title.x = element_blank(), 
                                         axis.text = element_text(size = 14), 
                                         axis.title = element_text(size = 14, face = "bold"), 
                                         plot.title = element_text(size = 20))
  })
  
  
  # Summary Statistics----------------------------------------------------------
  # Attendees as a proportion of those who registered                                        
  QUIZ_prop_attend <- function(){
    round((as.numeric(QUIZ_summary_statistics[4]) / 
             as.numeric(QUIZ_summary_statistics[2])) * 100, digits = 0)}
  output$QUIZ_prop_attend_box <- renderValueBox({
    valueBox(paste0(QUIZ_prop_attend(), "%"), 
             "Attendees as a proportion of those who registered", color = RAG(QUIZ_prop_attend() / 100))})
  
  # Number of attendees
  QUIZ_num_attendees <- function(){QUIZ_summary_statistics[4]}
  output$QUIZ_num_attendees_box <- renderValueBox({
    valueBox(paste0(QUIZ_num_attendees()), "Number of attendees", color = "light-blue")})
  
  # Number of registrations
  QUIZ_num_registrations <- function(){QUIZ_summary_statistics[2]}
  output$QUIZ_num_registrations_box <- renderValueBox({
    valueBox(paste0(QUIZ_num_registrations()), "Number of registrations", color = "light-blue")})
  
  # Proportion of people who viewed registration page that then signed up, (num registered + num cancelled) / num views
  QUIZ_prop_register <- function(){
    round(((as.numeric(QUIZ_summary_statistics[2]) + 
              as.numeric(QUIZ_summary_statistics[3])) / 
             as.numeric(QUIZ_summary_statistics[1])) * 100, digits = 0)}
  output$QUIZ_prop_register_box <- renderValueBox({
    valueBox(paste0(QUIZ_prop_register(), "%"), 
             "Proportion of people who viewed registration page that then signed up", 
             color = RAG(QUIZ_prop_register() / 100))}) 
  
  # Number of page views
  QUIZ_num_page_views <- function(){QUIZ_summary_statistics[1]}
  output$QUIZ_num_page_views_box <- renderValueBox({
    valueBox(paste0(QUIZ_num_page_views()), "Number of page views", color = "light-blue")})
  
  # Number of cancellations
  QUIZ_num_cancel <- function(){as.numeric(QUIZ_summary_statistics[3])}
  output$QUIZ_num_cancel_box <- renderValueBox({
    valueBox(paste0(QUIZ_num_cancel()), "Number of cancellations", color = "light-blue")})
  
  # Average attendance time
  QUIZ_avg_attend_time <- function(){QUIZ_summary_statistics[6]}
  output$QUIZ_avg_attend_time_box <- renderValueBox({
    valueBox(paste0(QUIZ_avg_attend_time()), "Average attendance duration", color = "light-blue")})
  
  # Training duration
  QUIZ_train_duration <- function(){"1h 00m 00s"}
  output$QUIZ_train_duration_box <- renderValueBox({
    valueBox(paste0(QUIZ_train_duration()), "Target training duration", color = "light-blue")})
  
  # Meeting duration
  QUIZ_meet_duration <- function(){QUIZ_summary_statistics[5]}
  output$QUIZ_meet_duration_box <- renderValueBox({
    valueBox(paste0(QUIZ_meet_duration()), "Actual training duration", color = "light-blue")})
  
  
} # end of server bracket 

# Run the application 
shinyApp(ui = ui, server = server)
