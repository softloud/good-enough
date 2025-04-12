#' Get JIRA Data
#'
#' Extract JIRA data as JSON using provided credentials and query.
#'
#' @param key_source Character. Where to get credentials: "env" (default), "file", or "manual".
#' @param key_path Character. Path to file (if `key_source == "file"`).
#' @param jira_email Character. Your JIRA account email (manual mode).
#' @param jira_api Character. Your JIRA API key (manual mode).
#' @param jira_url Character. Base URL of your JIRA instance (manual mode).
#'
#' @return A list containing JIRA data.
#' @importFrom httr GET authenticate content stop_for_status
#' @importFrom jsonlite fromJSON
#' @export
get_jira_data <- function(key_source = "env",
                          key_path = NULL,
                          jira_email = NULL,
                          jira_api = NULL,
                          jira_url = NULL) {

  if (key_source == "env") {
    jira_email <- Sys.getenv("JIRA_EMAIL")
    jira_api   <- Sys.getenv("JIRA_API_KEY")
    jira_url   <- Sys.getenv("JIRA_URL")

    if (jira_email == "" || jira_api == "" || jira_url == "") {
      stop("One or more environment variables (JIRA_EMAIL, JIRA_API_KEY, JIRA_URL) not found.")
    }

  } else if (key_source == "file") {
    if (is.null(key_path) || !file.exists(key_path)) {
      stop("Valid key_path must be provided for 'file' source.")
    }

    creds <- readLines(key_path, warn = FALSE)
    if (length(creds) < 3) {
      stop("Credential file must contain jira_email, jira_api, jira_url on separate lines.")
    }

    jira_email <- creds[1]
    jira_api   <- creds[2]
    jira_url   <- creds[3]

  } else if (key_source == "manual") {
    if (is.null(jira_email) || is.null(jira_api) || is.null(jira_url)) {
      stop("Manual source selected: jira_email, jira_api, and jira_url must all be provided.")
    }

  } else {
    stop("Invalid key_source. Choose 'env', 'file', or 'manual'.")
  }

  auth <- httr::authenticate(jira_email, jira_api)
  jql_query <- list(jql = "filter = everything")

  response <- httr::GET(jira_url, auth, query = jql_query)
  httr::stop_for_status(response)

  jira_data <- jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8"))
  return(jira_data)
}
