#setwd("~/Documents/R Files/Advanced R Programming (Coursera)")
#setwd("~/R/Coursera - Mastering Software Development R/Advanced R Programming")
library(readr)
library(magrittr)
library(dplyr)
library(tidyr)

# Inspect the dataset --------------------------------------------------------------------
dataset <- read_csv("MIE.csv")
unique(dataset$id)
unique(dataset$visit)
unique(dataset$room)
summary(dataset$value)
summary(dataset$timepoint)
str(dataset)

##########################################################################################
# Some notes To Self About S3 objects for this assignment from the book 
# Advanced R by Hadley Wickham that guided me through this assignment.

# S3: no formal definition of a class. can create class while creating a func with structure()
#   or assign class later with class()

# S3: METHODS belong to FUNCTIONS; METHODS do not belong to OBJECTS or CLASSES. HUGE DIFFERENCE than typical object orientation.

# S3: the job of an S3 generic method is to call the right method for a specified class. 
#   generic_method_name.class_name <- function()

# S3: S3 objects are usually built on top of lists or atomic vectors. I'm going to approach
#   this using lists. 

# Notes about needing different classes: need different classes b/c different generic methods
#   like print, summary may require different outputs depending on  make_LD(), subject(),
#   visit(), room(). We'll need different methods for these generics (print, summary) 
#   and we can do that by associating different classes for them.
##########################################################################################



##########################################################################################
# CHECKLIST
# 1. Create make_LD function to create a LongitudinalData class. 
#       a) LongitudinalData objects need a print method. print is existing generic. 
#       b) LongitudinalData objects need a subject method, assign a new subject class.
# 2. Create generic functions: subject, visit, room.
# 3. subject class
#       a) subject objects need a print method. print is existing generic. 
#       b) subject objects need a summary method, assign a special summary class, SummarizeLongitudinalData,
#           which we can then associate with a generic print method for summary objects.
#       d) subject objects need a visit method, assign a new visit class.
# 4. visit class
#       a) visit objects need a room method, assign a new room class.
#       b) visit objects could use a summary method, but not required for this assignment. pass.
# 5. room class
#       a) room objects need a print method. print is existing generic. 
#       b) room objects need a summary method, assign to SummarizeLongitudinalData class,
#           which we can associate with a generic print method for summary objects.
# 6. print.SummarizeLongitudinalData method
#       a) a generic method to print data summaries from SummarizeLongitudinalData objects
#           that print a specifically structured output. 
##########################################################################################




# Create main function to convert dataframes into a LongitudinalData class/object -------
make_LD <- function(x) {
    if (!is.data.frame(x)) {stop("X must be a data frame")}
    df_list <- split(x, x$id) # create a list with a sep. df for each subject. 
    structure(df_list, class = "LongitudinalData")
}

# Create Generic functions ---------------------------------------------------------------

# create generic methods for identifying subject, visit, and room. The creation of every 
# generic method users the UseMethod() function. After creating the generics, 
# Generic methods can return different values depending on the class of its input. 
# Do this with generic_method_name.class_name <- function(), seen later on. 

subject <- function(x, id) UseMethod("subject")

visit <- function(x, visit_num) UseMethod("visit")

room <- function(x, room_name) UseMethod("room")


# Methods for the LongitudinalData class -------------------------------------------------

#  print() is an existing generic. create special print method for LongitudinalData objects.
print.LongitudinalData <- function(x) {
    cat("Longitudinal dataset with", length(x), "subjects")
}

print(temp)
print(class(temp))


# Create a subject method for LongitudinalData objects; also assign it a new class "subject"
subject.LongitudinalData <- function(x, id) {
    subject_data <- x[[as.character(id)]] # list subset.
    structure(list(id = id,
                   dataset = subject_data), class = "subject")
}

# Methods for the subject class ----------------------------------------------------------

# note: x is expected to be a list of the single selected subject. 
print.subject <- function(x) {
    if(is.null(x[["dataset"]])) {
        NULL
    } else {
        cat("Subject ID:", x[["id"]])
    }
}

summary.subject <- function(x) {
    if(!require(dplyr)) {
        message("installing 'dplyr' package")
        install.packages('dplyr')
    } 
    if(!require(tidyr)) {
        message("installing 'tidyr' package")
        install.packages('tidyr')
    }
    
    subject_summary <- x[["dataset"]] %>%
        group_by(visit, room) %>%
        summarize(mean_pollution = mean(value)) %>%
        spread(key = room, value = mean_pollution) %>%
        as.data.frame() # coerce to just a vanilla data frame. 
    
    # to create a special print method for anything being summarized
    structure(list(id = x[["id"]],
                   dataset = subject_summary), class = "SummarizeLongitudinalData")
}

# a visit method for  subject objects. also assign it a new class "visit"
visit.subject <- function(x, visit_num) {  # x should be a subject class. 
    visit_data <- x[["dataset"]] %>%
        filter(visit == visit_num)
    structure(list(id = x[["id"]],
                   visit = visit_num,
                   dataset = visit_data), class = "visit")
}

# Methods for the visit class ------------------------------------------------------------

# a room method for the visit class to extract data of a single room number for that
# visit. also assign it a new class "room" 
room.visit <- function(x, room_name) {
    room_data <- x[["dataset"]] %>%
        filter(room == room_name)
    structure(list(id = x[["id"]],
                   visit = x[["visit"]],
                   room = room_name,
                   dataset = room_data), class = "room")
}

# Methods for the room class -------------------------------------------------------------

# note: x is expected to be a room object. 
print.room <- function(x) {
    if(is.null(x[["dataset"]])) {
        NULL
    } else {
        cat("ID: ", x[["id"]], "\n")
        cat("Visit: ", x[["visit"]], "\n")
        cat("Room: ", x[["room"]])
    }
}

summary.room <- function(x) { # x should be a room object. 
    room_summary <- summary(x[["dataset"]]$value)
    structure(list(id = x[["id"]],
                   dataset = room_summary), class = "SummarizeLongitudinalData")
}

# Print method for SummarizeLongitudinalData class ---------------------------------------

# this should cover all print needs for visit and room summary's of the 
# SummarizeLongitudinalData class.
print.SummarizeLongitudinalData <- function(x) {
    cat("ID: ", x[["id"]], '\n')
    x[["dataset"]]
}



# TEST OUT THE METHODS FOR INTERACTING WITH LONGITUDINALDATA -----------------------------
dataset <- read_csv("MIE.csv")
x <- make_LD(dataset)
print(class(x))
print(x)

## Subject 10 doesn't exist
out <- subject(x, 10)
print(out)

out <- subject(x, 14)
print(out)

out <- subject(x, 54) %>% summary
print(out)

out <- subject(x, 14) %>% summary
print(out)

out <- subject(x, 44) %>% visit(0) %>% room("bedroom")
print(out)

## Show a summary of the pollutant values
out <- subject(x, 44) %>% visit(0) %>% room("bedroom") %>% summary
print(out)

out <- subject(x, 44) %>% visit(1) %>% room("living room") %>% summary
print(out)
