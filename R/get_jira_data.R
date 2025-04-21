#' Get JIRA data
#' 
#' Extract JIRA data as a json.

library(httr)
library(jsonlite)
# source("jiraapi.R")

get_jira_data <- function(jira_email, jira_api) {

  auth <- authenticate(jira_email, jira_api)
  jql_query <- list(jql = "filter = everything")

  response <- GET(jira_url, auth, query = jql_query)
  stop_for_status(response)

  jira_data <- fromJSON(content(response, "text", encoding = "UTF-8"))

}