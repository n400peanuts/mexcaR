# LIST OF REQUIRED PACKAGES -----------------------------------------------

required_packages <- c(
  "checkpoint"
)

# install missing packages

new.packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]

if (length(new.packages)) {
  install.packages(new.packages)
}

rm(new.packages)

library(checkpoint)
checkpoint(snapshot_date ='2022-08-17', r_version = '4.2.0')
library(shiny)
library(tidyverse)
library(av)
library(magick)
library(ggimage)
library(ggmap)
library(parallel)
library(shinyFeedback)
library(shinycustomloader)
library(waiter)