library(shiny)
library(shinydashboard)
library(dashboardthemes)
library(DT)
library(ggplot2)
library(GGally)
library(tidyverse)
library(shinythemes)
library(plotly)
library(rgl)
library(pca3d)
library(tree)
library(readxl)
library(rsconnect)
library(MASS)
library(rugarch)

######################################################
#          UI file for the shiny application         #
#                Author: Zheng Liu                   #
######################################################

# ready for rgl plot
options(rgl.useNULL = TRUE)


dashboardPage(skin = "blue",
              dashboardHeader(title = "SOFR"),
              dashboardSidebar(uiOutput("ui_sidebar")),
              dashboardBody(
                  shinyDashboardThemes(
                      theme = "blue_gradient"
                  ),
                  uiOutput("ui_body")
              )
)