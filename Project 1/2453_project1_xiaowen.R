# 2453_project1.R
#
# Purpose: web scraping of job hunting webstie
# Version: 1.0
# Version history:
# Date: 2020-11-10
# Author: Xiaowen
# License:
#
# Input:
# Output:
# Dependencies: Use Indeed website
#
# ToDo:
# 1. extract job descriptions with salaries for different industries
# 2. data cleaning and date wangling.
# 3. NLP

# Notes:
#
# ==============================================================================

# =========== 1. Extract Data ======================== #
# =========== Extract Job Descriptions =============== #

# load required  libraries
library(tidyverse)
library(rvest)

# Indeed: job website - filling form scraping

# the url for the session
url <- "https://ca.indeed.com/"


# We can simulate a session in a browser
html <- url %>% 
  html_session() # tml_session() is like opening a browser on that webpage at the computer backside

# extract forms from the webpage
form_blank <- url %>% 
  read_html() %>% 
  html_form()

# What does it look like when we print it
print(form_blank) # q is question, l is location

# What fields are in this form
print(form_blank[[1]]$fields)


# fill in the form with "q" and "l"
form_filled = form_blank[[1]] %>%
  set_values(
    "q" = "Statistics",
    "l" = "Toronto, ON") 
# Can change queries and locations



# submit the form
submitted <- html %>% 
  submit_form(form_filled)

print(submitted)

job_locations <- submitted %>% 
  rvest::html_nodes(".location") %>%
  rvest::html_text()

# find all job titles
job_title <- submitted %>% # from the submitted form
  rvest::html_nodes("div") %>% # get the div nodes
  rvest::html_nodes(xpath = '//*[@data-tn-element = "jobTitle"]') %>% # find nodes where data-tn-element = "jobTitle" (look for this in any span, div, a tag, p tag...)
  rvest::html_attr("title") # extract the title attribute

job_title

job_company <- submitted %>% 
  rvest::html_nodes(".company") %>%
  rvest::html_text() %>%
  stringi::stri_trim_both()

job_links <- submitted %>% 
  rvest::html_nodes("div") %>%
  rvest::html_nodes(xpath = '//*[@data-tn-element="jobTitle"]') %>%
  rvest::html_attr("href")

# we can pull the summary blurb of the job description by 
# extracting the summary class

job_blurb <- submitted %>%
  rvest::html_nodes('.summary') %>% 
  rvest::html_text() %>%
  stringi::stri_trim_both()

jobs_data <- tibble(title = job_title,
                    company = job_company,
                    location = job_locations,
                    links = job_links,
                    blurb = job_blurb)

DT::datatable(jobs_data, caption = "scrapped jobs",
              options = list(scrollX = T))


job_des <- vector(mode="character", length=nrow(jobs_data))

for (i in 1:nrow(jobs_data)){
  job <- submitted %>% jump_to(jobs_data$links[i]) ## go to the page for the ith job description
  
  ## pull the text of the job descripion
  job_des[i] <- job %>%    
    html_nodes('div') %>% 
    html_nodes('#jobDescriptionText') %>% 
    html_text()
  
}

job_des




