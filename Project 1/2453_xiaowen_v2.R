# 2453_project1.R
#
# Purpose: web scraping of job hunting webste
# Version: 2.0 (formal version)
# Version history: 1.0 (more like a draft)
# Date: 2020-11-14
# Author: Xiaowen
# ==============================================================================

# required libraries
library(tidyverse)
library(rvest)

# Functions; Examples are at the end


# form filling
form_filling <- function(job_query, location){
  # input: job_query and location
  # output: the submitted form html
  
  
  # the url for the session
  url <- "https://ca.indeed.com/"
  
  html <- url %>% 
    html_session() # html_session() is like opening a browser on that webpage at the computer backside
  
  # extract forms from the webpage
  form_blank <- url %>% 
    read_html() %>% 
    html_form()
  
  # fill in the form with "q" and "l"
  form_filled = form_blank[[1]] %>%
    set_values(
      "q" = job_query,
      "l" = location) 
  
  # submit the form
  submitted <- html %>% 
    submit_form(form_filled)
  
  return(submitted)
}


get_job_data <- function(submitted){
  
  # input: url
  # output: job searching data table with job title, company, location, link, blurb 
  
  # find job_locations
  job_locations <- submitted %>% 
    rvest::html_nodes(".location") %>%
    rvest::html_text()
  
  # find all job titles
  job_title <- submitted %>% # from the submitted form
    rvest::html_nodes("div") %>% # get the div nodes
    rvest::html_nodes(xpath = '//*[@data-tn-element = "jobTitle"]') %>% # find nodes where data-tn-element = "jobTitle" (look for this in any span, div, a tag, p tag...)
    rvest::html_attr("title") # extract the title attribute
  
  # job companies
  job_company <- submitted %>% 
    rvest::html_nodes(".company") %>%
    rvest::html_text() %>%
    stringi::stri_trim_both()
  
  # job links
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
  
  # wrap up all job data
  jobs_data <- tibble(title = job_title,
                      company = job_company,
                      location = job_locations,
                      links = job_links,
                      blurb = job_blurb)
  
  return(jobs_data)
}


get_job_des <- function(submitted, jobs_data){
  # input: jobs data from the submitted
  # output: string vector of all job descriptions of the query
  
  job_des <- vector(mode="character", length=nrow(jobs_data))
  
  for (i in 1:nrow(jobs_data)){
    job <- submitted %>% jump_to(jobs_data$links[i]) ## go to the page for the ith job description
    
    ## pull the text of the job descripion
    job_des[i] <- job %>%    
      html_nodes('div') %>% 
      html_nodes('#jobDescriptionText') %>% 
      html_text()
    
  }
  
  return(job_des)
  
  
}

get_next_page_data <- function(job_query, location, n){
  # input: query, location and n indicating which page; n = 10 is the second page, n=20 is the third, and so on.
  # output: the data on the second/third/... page.
  # Require: n has to be 10, 20, 30, ...
  
  base_url <- form_filling(job_query, location)$url
  suffix <- glue::glue("&start={n}")
  
  url <- paste0(base_url, suffix)
  page_data <- get_job_data(html(url))
  
  return(page_data)
  
  
}


# generate file name with corresponding queries
file_name <- function(job_query, location){
  location <- gsub(" ", "", location, fixed = TRUE) 
  return(paste0(job_query, "_", location, ".csv"))
}

# write_csv(total_data, file_name(query1, loc1))

# ================ Current examples ===========================

query1 = "Sales"
loc1 = "Toronto, ON"

submit1 <- form_filling(query1, loc1)
data1 <- get_job_data(submit1)

get_job_des(submit1, data1)

# Loop: combine all job_data and 10 pages data together
# problem: may be more than or less than 10 pages
n = seq(from = 10, to = 100, by = 10)
first_page_data <- get_job_data(submit1) # job data on the first page

for (i in 1:length(n)){
  n = n[i]
  total_data <- rbind(first_page_data, get_next_page_data(query1, loc1, n))
}

write_csv(total_data, file_name(query1, loc1))


# =================== Unsolved problems =========================

# 1. get_job_des function only works for the output of form_filling(.);
# but not for the get_next_page_data(.)

# 2. now, I loop 10 pages and combine to the total data; should include all  
# searching data (should consider the cases where there are more than 10 pages or only 1 page).

