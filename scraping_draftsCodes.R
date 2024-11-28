# Load necessary libraries
library(tidyverse)
library(rvest)
library(dplyr)
library(jsonlite)
library(RSelenium)
library(chromote)
library(purrr)
library(wdman)
library(googlesheets4)
library(googledrive)

##################################################
# scraping active page using chromote #
#############################################

# URL of the webpage to scrape
url <- "https://fedme.results.info/event/30/general/lead"
############   chromote:: OPTION ######
# Create a new Chromote session
b <- ChromoteSession$new()
# Navigate to the webpage
b$Page$navigate(url)
# Extract the rendered HTML
html <- b$Runtime$evaluate('document.querySelector(".mx-2:nth-child(1)").click();')  # Replace with your CSS selector
Sys.sleep(10) 

### WAIT for "backstage" page loading ####
html <- b$Runtime$evaluate("document.documentElement.outerHTML")$result$value
## 
page <- read_html(html)
### 
compe <- page %>%
  html_elements('div.gr-event-name') %>%
  html_text()
###
competitor_divs <- page %>% 
  html_elements("div.athlete-basic-line") # Adjust the selector based on the HTML structure
###
compe
# Extract and clean data
scrap <- competitor_divs %>%
  map_df(~{
    rank <- .x %>% html_element(".rank") %>% html_text(trim = TRUE) # Adjust class
    name <- .x %>% html_element(".r-name") %>% html_text(trim = TRUE) # Adjust class
    score <- .x %>% html_element(".score") %>% html_text(trim = FALSE) # score
    #score.clasif <- .x %>% html_element(".score quali cell") %>% html_text(trim = TRUE) # score
    team <- .x %>% html_element("span:nth-child(2)") %>% html_text(trim = TRUE) # Team
    
    # Return as a tibble
    tibble(Name = name, 
           Rank = rank, 
           Score = score, 
           #Clasific = score.clasif,
           Team = team)
  })
###
scrap <- scrap %>% 
  separate(Score, into = c(NA, "score", "score.rank", NA), sep = " ")
scrap <- scrap %>% 
  separate(score.rank, into = c(NA,NA,"score.rank"), sep = "()")
scrap <- scrap %>% 
  mutate(Rank = as.numeric(Rank))
scrap$Category <- rep("Abs_F", length(scrap$Name) )
scrap$Competition <- rep(compe, length(scrap$Name) )
##
compe
n.competitors <- max(scrap$Rank, na.rm = TRUE); n.competitors
##
# scrap %>% print(n=length(scrap$Name))
scrap[,c(1:6)] %>% 
  filter(!is.na(score)) %>%
  print(n = 17)
############### ##################
Abs_F <- scrap

##################################################
# scraping active page using chromote #
#############################################

# AbsF DOne
# AbsM

# URL of the webpage to scrape
url <- "https://fedme.results.info/event/30/general/lead"
############   chromote:: OPTION ######
# Create a new Chromote session
b <- ChromoteSession$new()
# Navigate to the webpage
b$Page$navigate(url)
# Extract the rendered HTML
html <- b$Runtime$evaluate('document.querySelector(".mx-2:nth-child(2)").click();')  # Replace with your CSS selector
Sys.sleep(10) 

### WAIT for "backstage" page loading ####
html <- b$Runtime$evaluate("document.documentElement.outerHTML")$result$value
## 
page <- read_html(html)
### 
compe <- page %>%
  html_elements('div.gr-event-name') %>%
  html_text()
###
competitor_divs <- page %>% 
  html_elements("div.athlete-basic-line") # Adjust the selector based on the HTML structure
###
compe
# Extract and clean data
scrap <- competitor_divs %>%
  map_df(~{
    rank <- .x %>% html_element(".rank") %>% html_text(trim = TRUE) # Adjust class
    name <- .x %>% html_element(".r-name") %>% html_text(trim = TRUE) # Adjust class
    score <- .x %>% html_element(".score") %>% html_text(trim = FALSE) # score
    #score.clasif <- .x %>% html_element(".score quali cell") %>% html_text(trim = TRUE) # score
    team <- .x %>% html_element("span:nth-child(2)") %>% html_text(trim = TRUE) # Team
    
    # Return as a tibble
    tibble(Name = name, 
           Rank = rank, 
           Score = score, 
           #Clasific = score.clasif,
           Team = team)
  })
###
scrap <- scrap %>% 
  separate(Score, into = c(NA, "score", "score.rank", NA), sep = " ")
scrap <- scrap %>% 
  separate(score.rank, into = c(NA,NA,"score.rank"), sep = "()")
scrap <- scrap %>% 
  mutate(Rank = as.numeric(Rank))
scrap$Category <- rep("Abs_M", length(scrap$Name) )
scrap$Competition <- rep(compe, length(scrap$Name) )
##
compe
n.competitors <- max(scrap$Rank, na.rm = TRUE); n.competitors
##
# scrap %>% print(n=length(scrap$Name))
scrap[,c(1:6)] %>% 
  filter(!is.na(score)) %>%
  print(n = 17)
############### ##################
Abs_M <- scrap
#################################

# scraping active page using chromote #
#############################################

# AbsF DOne
# AbsM DONE
# Sub18M

# URL of the webpage to scrape
url <- "https://fedme.results.info/event/30/general/lead"
############   chromote:: OPTION ######
# Create a new Chromote session
b <- ChromoteSession$new()
# Navigate to the webpage
b$Page$navigate(url)
# Extract the rendered HTML
html <- b$Runtime$evaluate('document.querySelector(".mx-2:nth-child(3)").click();')  # Replace with your CSS selector
Sys.sleep(10) 

### WAIT for "backstage" page loading ####
html <- b$Runtime$evaluate("document.documentElement.outerHTML")$result$value
## 
page <- read_html(html)
### 
compe <- page %>%
  html_elements('div.gr-event-name') %>%
  html_text()
###
competitor_divs <- page %>% 
  html_elements("div.athlete-basic-line") # Adjust the selector based on the HTML structure
###
compe
# Extract and clean data
scrap <- competitor_divs %>%
  map_df(~{
    rank <- .x %>% html_element(".rank") %>% html_text(trim = TRUE) # Adjust class
    name <- .x %>% html_element(".r-name") %>% html_text(trim = TRUE) # Adjust class
    score <- .x %>% html_element(".score") %>% html_text(trim = FALSE) # score
    #score.clasif <- .x %>% html_element(".score quali cell") %>% html_text(trim = TRUE) # score
    team <- .x %>% html_element("span:nth-child(2)") %>% html_text(trim = TRUE) # Team
    
    # Return as a tibble
    tibble(Name = name, 
           Rank = rank, 
           Score = score, 
           #Clasific = score.clasif,
           Team = team)
  })
###
scrap <- scrap %>% 
  separate(Score, into = c(NA, "score", "score.rank", NA), sep = " ")
scrap <- scrap %>% 
  separate(score.rank, into = c(NA,NA,"score.rank"), sep = "()")
scrap <- scrap %>% 
  mutate(Rank = as.numeric(Rank))
scrap$Category <- rep("Sub18_M", length(scrap$Name) )
scrap$Competition <- rep(compe, length(scrap$Name) )
##
compe
n.competitors <- max(scrap$Rank, na.rm = TRUE); n.competitors
##
# scrap %>% print(n=length(scrap$Name))
scrap[,c(1:6)] %>% 
  filter(!is.na(score)) %>%
  print(n = 17)
############### ##################
Sub18M <- scrap
#################################
# AbsF DONE
# AbsM DONE
# Sub18 M DONE
# Sub18 F
# Sub16 M
# Sub16 F
# Sub14 M
# Sub14 F

# scraping active page using chromote #
#############################################

# AbsF DOne
# AbsM DONE
# Sub18M DONE
# Sub18F 

# URL of the webpage to scrape
url <- "https://fedme.results.info/event/30/general/lead"
############   chromote:: OPTION ######
# Create a new Chromote session
b <- ChromoteSession$new()
# Navigate to the webpage
b$Page$navigate(url)
# Extract the rendered HTML
html <- b$Runtime$evaluate('document.querySelector(".mx-2:nth-child(4)").click();')  # Replace with your CSS selector
Sys.sleep(10) 

### WAIT for "backstage" page loading ####
html <- b$Runtime$evaluate("document.documentElement.outerHTML")$result$value
## 
page <- read_html(html)
### 
compe <- page %>%
  html_elements('div.gr-event-name') %>%
  html_text()
###
competitor_divs <- page %>% 
  html_elements("div.athlete-basic-line") # Adjust the selector based on the HTML structure
###
compe
# Extract and clean data
scrap <- competitor_divs %>%
  map_df(~{
    rank <- .x %>% html_element(".rank") %>% html_text(trim = TRUE) # Adjust class
    name <- .x %>% html_element(".r-name") %>% html_text(trim = TRUE) # Adjust class
    score <- .x %>% html_element(".score") %>% html_text(trim = FALSE) # score
    #score.clasif <- .x %>% html_element(".score quali cell") %>% html_text(trim = TRUE) # score
    team <- .x %>% html_element("span:nth-child(2)") %>% html_text(trim = TRUE) # Team
    
    # Return as a tibble
    tibble(Name = name, 
           Rank = rank, 
           Score = score, 
           #Clasific = score.clasif,
           Team = team)
  })
###
scrap <- scrap %>% 
  separate(Score, into = c(NA, "score", "score.rank", NA), sep = " ")
scrap <- scrap %>% 
  separate(score.rank, into = c(NA,NA,"score.rank"), sep = "()")
scrap <- scrap %>% 
  mutate(Rank = as.numeric(Rank))
scrap$Category <- rep("Sub18_F", length(scrap$Name) )
scrap$Competition <- rep(compe, length(scrap$Name) )
##
compe
n.competitors <- max(scrap$Rank, na.rm = TRUE); n.competitors
##
# scrap %>% print(n=length(scrap$Name))
scrap[,c(1:6)] %>% 
  filter(!is.na(score)) %>%
  print(n = 17)
############### ##################
Sub18F <- scrap

# scraping active page using chromote #
#############################################

# AbsF DOne
# AbsM DONE
# Sub18M DONE
# Sub18F DONE
# Sub16M

# URL of the webpage to scrape
url <- "https://fedme.results.info/event/30/general/lead"
############   chromote:: OPTION ######
# Create a new Chromote session
b <- ChromoteSession$new()
# Navigate to the webpage
b$Page$navigate(url)
# Extract the rendered HTML
html <- b$Runtime$evaluate('document.querySelector(".mx-2:nth-child(5)").click();')  # Replace with your CSS selector
Sys.sleep(10) 

### WAIT for "backstage" page loading ####
html <- b$Runtime$evaluate("document.documentElement.outerHTML")$result$value
## 
page <- read_html(html)
### 
compe <- page %>%
  html_elements('div.gr-event-name') %>%
  html_text()
###
competitor_divs <- page %>% 
  html_elements("div.athlete-basic-line") # Adjust the selector based on the HTML structure
###
compe
# Extract and clean data
scrap <- competitor_divs %>%
  map_df(~{
    rank <- .x %>% html_element(".rank") %>% html_text(trim = TRUE) # Adjust class
    name <- .x %>% html_element(".r-name") %>% html_text(trim = TRUE) # Adjust class
    score <- .x %>% html_element(".score") %>% html_text(trim = FALSE) # score
    #score.clasif <- .x %>% html_element(".score quali cell") %>% html_text(trim = TRUE) # score
    team <- .x %>% html_element("span:nth-child(2)") %>% html_text(trim = TRUE) # Team
    
    # Return as a tibble
    tibble(Name = name, 
           Rank = rank, 
           Score = score, 
           #Clasific = score.clasif,
           Team = team)
  })
###
scrap <- scrap %>% 
  separate(Score, into = c(NA, "score", "score.rank", NA), sep = " ")
scrap <- scrap %>% 
  separate(score.rank, into = c(NA,NA,"score.rank"), sep = "()")
scrap <- scrap %>% 
  mutate(Rank = as.numeric(Rank))
scrap$Category <- rep("Sub16_M", length(scrap$Name) )
scrap$Competition <- rep(compe, length(scrap$Name) )
##
compe
n.competitors <- max(scrap$Rank, na.rm = TRUE); n.competitors
##
# scrap %>% print(n=length(scrap$Name))
scrap[,c(1:6)] %>% 
  filter(!is.na(score)) %>%
  print(n = 17)
############### ##################
Sub16M <- scrap

# scraping active page using chromote #
#############################################

# AbsF DOne
# AbsM DONE
# Sub18M DONE
# Sub18F DONE
# Sub16M DONE
# Sub16F 

# URL of the webpage to scrape
url <- "https://fedme.results.info/event/30/general/lead"
############   chromote:: OPTION ######
# Create a new Chromote session
b <- ChromoteSession$new()
Sys.sleep(3)
# Navigate to the webpage
b$Page$navigate(url)
Sys.sleep(3)
# Extract the rendered HTML
html <- b$Runtime$evaluate('document.querySelector(".mx-2:nth-child(6)").click();')  # Replace with your CSS selector
Sys.sleep(10) 

### WAIT for "backstage" page loading ####
html <- b$Runtime$evaluate("document.documentElement.outerHTML")$result$value
## 
page <- read_html(html)
### 
compe <- page %>%
  html_elements('div.gr-event-name') %>%
  html_text()
###
competitor_divs <- page %>% 
  html_elements("div.athlete-basic-line") # Adjust the selector based on the HTML structure
###
compe
# Extract and clean data
scrap <- competitor_divs %>%
  map_df(~{
    rank <- .x %>% html_element(".rank") %>% html_text(trim = TRUE) # Adjust class
    name <- .x %>% html_element(".r-name") %>% html_text(trim = TRUE) # Adjust class
    score <- .x %>% html_element(".score") %>% html_text(trim = FALSE) # score
    #score.clasif <- .x %>% html_element(".score quali cell") %>% html_text(trim = TRUE) # score
    team <- .x %>% html_element("span:nth-child(2)") %>% html_text(trim = TRUE) # Team
    
    # Return as a tibble
    tibble(Name = name, 
           Rank = rank, 
           Score = score, 
           #Clasific = score.clasif,
           Team = team)
  })
###
scrap <- scrap %>% 
  separate(Score, into = c(NA, "score", "score.rank", NA), sep = " ")
scrap <- scrap %>% 
  separate(score.rank, into = c(NA,NA,"score.rank"), sep = "()")
scrap <- scrap %>% 
  mutate(Rank = as.numeric(Rank))
scrap$Category <- rep("Sub16_F", length(scrap$Name) )
scrap$Competition <- rep(compe, length(scrap$Name) )
##
compe
n.competitors <- max(scrap$Rank, na.rm = TRUE); n.competitors
##
# scrap %>% print(n=length(scrap$Name))
scrap[,c(1:6)] %>% 
  filter(!is.na(score)) %>%
  print(n = 17)
############### ##################
Sub16F <- scrap

# scraping active page using chromote #
#############################################

# AbsF DOne
# AbsM DONE
# Sub18M DONE
# Sub18F DONE
# Sub16M DONE
# Sub16F DONE
# Sub14M 

# URL of the webpage to scrape
url <- "https://fedme.results.info/event/30/general/lead"
############   chromote:: OPTION ######
# Create a new Chromote session
b <- ChromoteSession$new()
# Navigate to the webpage
b$Page$navigate(url)
# Extract the rendered HTML
html <- b$Runtime$evaluate('document.querySelector(".mx-2:nth-child(7)").click();')  # Replace with your CSS selector
Sys.sleep(10) 

### WAIT for "backstage" page loading ####
html <- b$Runtime$evaluate("document.documentElement.outerHTML")$result$value
## 
page <- read_html(html)
### 
compe <- page %>%
  html_elements('div.gr-event-name') %>%
  html_text()
###
competitor_divs <- page %>% 
  html_elements("div.athlete-basic-line") # Adjust the selector based on the HTML structure
###
compe
# Extract and clean data
scrap <- competitor_divs %>%
  map_df(~{
    rank <- .x %>% html_element(".rank") %>% html_text(trim = TRUE) # Adjust class
    name <- .x %>% html_element(".r-name") %>% html_text(trim = TRUE) # Adjust class
    score <- .x %>% html_element(".score") %>% html_text(trim = FALSE) # score
    #score.clasif <- .x %>% html_element(".score quali cell") %>% html_text(trim = TRUE) # score
    team <- .x %>% html_element("span:nth-child(2)") %>% html_text(trim = TRUE) # Team
    
    # Return as a tibble
    tibble(Name = name, 
           Rank = rank, 
           Score = score, 
           #Clasific = score.clasif,
           Team = team)
  })
###
scrap <- scrap %>% 
  separate(Score, into = c(NA, "score", "score.rank", NA), sep = " ")
scrap <- scrap %>% 
  separate(score.rank, into = c(NA,NA,"score.rank"), sep = "()")
scrap <- scrap %>% 
  mutate(Rank = as.numeric(Rank))
scrap$Category <- rep("Sub14_M", length(scrap$Name) )
scrap$Competition <- rep(compe, length(scrap$Name) )
##
compe
n.competitors <- max(scrap$Rank, na.rm = TRUE); n.competitors
##
# scrap %>% print(n=length(scrap$Name))
scrap[,c(1:6)] %>% 
  filter(!is.na(score)) %>%
  print(n = 17)
############### ##################
Sub14M <- scrap

# scraping active page using chromote #
#############################################

# AbsF DOne
# AbsM DONE
# Sub18M DONE
# Sub18F DONE
# Sub16M DONE
# Sub16F DONE
# Sub14M DONE
# Sub14F 

# URL of the webpage to scrape
url <- "https://fedme.results.info/event/30/general/lead"
############   chromote:: OPTION ######
# Create a new Chromote session
b <- ChromoteSession$new()
# Navigate to the webpage
b$Page$navigate(url)
# Extract the rendered HTML
html <- b$Runtime$evaluate('document.querySelector(".mx-2:nth-child(8)").click();')  # Replace with your CSS selector
Sys.sleep(10) 

### WAIT for "backstage" page loading ####
html <- b$Runtime$evaluate("document.documentElement.outerHTML")$result$value
## 
page <- read_html(html)
### 
compe <- page %>%
  html_elements('div.gr-event-name') %>%
  html_text()
###
competitor_divs <- page %>% 
  html_elements("div.athlete-basic-line") # Adjust the selector based on the HTML structure
###
compe
# Extract and clean data
scrap <- competitor_divs %>%
  map_df(~{
    rank <- .x %>% html_element(".rank") %>% html_text(trim = TRUE) # Adjust class
    name <- .x %>% html_element(".r-name") %>% html_text(trim = TRUE) # Adjust class
    score <- .x %>% html_element(".score") %>% html_text(trim = FALSE) # score
    #score.clasif <- .x %>% html_element(".score quali cell") %>% html_text(trim = TRUE) # score
    team <- .x %>% html_element("span:nth-child(2)") %>% html_text(trim = TRUE) # Team
    
    # Return as a tibble
    tibble(Name = name, 
           Rank = rank, 
           Score = score, 
           #Clasific = score.clasif,
           Team = team)
  })
###
scrap <- scrap %>% 
  separate(Score, into = c(NA, "score", "score.rank", NA), sep = " ")
scrap <- scrap %>% 
  separate(score.rank, into = c(NA,NA,"score.rank"), sep = "()")
scrap <- scrap %>% 
  mutate(Rank = as.numeric(Rank))
scrap$Category <- rep("Sub14_F", length(scrap$Name) )
scrap$Competition <- rep(compe, length(scrap$Name) )
##
compe
n.competitors <- max(scrap$Rank, na.rm = TRUE); n.competitors
##
# scrap %>% print(n=length(scrap$Name))
scrap[,c(1:6)] %>% 
  filter(!is.na(score)) %>%
  print(n = 17)
############### ##################
Sub14F <- scrap


##################################
############## ##################
### Write google sheet ###
First.Lead_ESP.24 <- rbind(Abs_F, 
             Abs_M, 
             Sub18M, 
             Sub18F,
             Sub16M,
             Sub16F,
             Sub14M,
             Sub14F)
First.Lead_ESP.24 %>% print(n=length(First.Lead_ESP.24$Name))

options(gargle_oauth_email = "villatoropazfj@dataanalysislab.com")
gs4_deauth()
gs4_auth()
sheet_write(data = First.Lead_ESP.24,
            ss= "https://docs.google.com/spreadsheets/d/1kXYUH9_QWNBbdIhVkgrthXAg2SUC5p6-rB651HlyG6k/edit?usp=sharing", 
            sheet=paste("First.Lead_ESP.24", "_", Sys.Date()))
