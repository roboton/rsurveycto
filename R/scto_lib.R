# library(curl)
# library(fs)
# library(jsonlite)
# library(lubridate)
# library(readr)
# library(stringr)

#' Get an authenticated CSRF token
#'
#' Helper function to authenticate with SurveyCTO and fetch corresponding CSRF
#' token
#' @param servername character: name of the SurveyCTO server.
#' @param username character: username for the account used to login to the server.
#' @param password character: password for the account used to login to the server.
#' @return An authenticated CSRF token
get_authenticated_csrf <- function(servername, username, password) {
  index_url <- stringr::str_glue("https://{servername}.surveycto.com/index.html")
  index_res <- httr::GET(index_url)
  csrf_token <- httr::headers(index_res)$`x-csrf-token`

  login_url <- stringr::str_glue(
    "https://{servername}.surveycto.com/login?spring-security-redirect=%2F")
  login_res <- httr::POST(
    login_url,
    body = list(username = username,
                password = password,
                csrf_token = csrf_token), encode = "form")

  return(csrf_token)
}

#' Get a SurveyCTO authentication session object
#'
#' Authenticates with SurveyCTO and fetches corresponding credentials
#' @param servername character: name of the SurveyCTO server.
#' @param username character: username for the account used to login to the server.
#' @param password character: password for the account used to login to the server.
#' @return Authenticated SurveyCTO session boject
#' @export
scto_auth <- function(servername, username, password) {
  host <- stringr::str_glue("https://{servername}.surveycto.com")

  curl_handle <- curl::new_handle()
  curl::handle_setopt(
      handle = curl_handle,
      httpauth = 1,
      userpwd = stringr::str_glue("{username}:{password}"))

  csrf_token <- get_authenticated_csrf(servername, username, password)

  return(list(host = host, curl_handle = curl_handle, csrf_token = csrf_token,
              servername = servername))
}

#' Download SurveyCTO data using the API
#'
#' This function pulls data from SurveyCTO using their api.
#' @param dataset_id character: SurveyCTO Dataset ID.
#' @param type character: "form" or "dataset" type
#' @param scto_auth an SurveyCTO authentication object
#' @param start_dt character: start datetime to begin fetching data (inclusive) YYYY-MM-DD HH:MM:SS
#' @param refresh logical: whether or not to pull refreshed data or use local cache
#' @param cache_dir character: path to store locally cached data
#' @return A dataframe containing the data in wide format.
#' @export
scto_pull <- function(dataset_id, type = "form", scto_auth, start_dt = 1,
                          refresh = FALSE, cache_dir = NULL) {

  start_dt <- as.integer(lubridate::as_datetime(start_dt))
  local_file <- fs::path(
    cache_dir,
    stringr::str_glue("{scto_auth$servername}_{id}_{type}_{start_dt}.rds"))

  if (fs::file_exists(local_file) && !refresh) {
    return(readr::read_rds(local_file))
  }

  request_url <- stringr::str_glue("{scto$host}/api/v2")
  if (type == "form") {
    request_url <- stringr::str_glue(
      "{request_url}/forms/data/wide/json/{id}?date={start_dt}")
  } else {
    request_url <- stringr::str_glue(
      "{request_url}/datasets/data/csv/{id}")
  }
  response <- curl::curl_fetch_memory(request_url, handle = scto_auth)
  status <- response$status_code
  content <- rawToChar(response$content)
  if (status == 200) {
    fs::dir_create(cache_dir, recurse = TRUE)
    if (type == "form") {
      scto_data <- as.data.frame(jsonlite::fromJSON(content, flatten = TRUE))
      readr::write_rds(scto_data, local_file)
      return(scto_data)
    } else {
      scto_data <- readr::read_csv(content, show_col_types = FALSE)
      readr::write_rds(scto_data, local_file)
      return(scto_data)
    }
  } else {
    print(stringr::str_glue("Response content:\n{content}"))
    stop(stringr::str_glue("Non-200 response: {status}"))
  }
}

#' Upload data to SurveyCTO
#'
#' This function uploads a csv file to SurveyCTO using web POSTs and GETs to
#' replace data in an existing Server Dataset
#' @param data character: data frame to upload
#' @param dataset_id character: dataset ID of existing SCTO Server Dataset
#' @param dataset_title character: dataset title of existing SCTO Server Dataset
#' @param scto_auth an SurveyCTO authentication object from scto_auth function
#' @return A dataframe containing the data in wide format.
#' @export
scto_push <- function(data, dataset_id, dataset_title, scto_auth) {
  # TODO: potential function arguments that need to be tested/validated before
  # turning into actual function arguments.
  dataset_exists <- TRUE # possible to upload to non-existant datasets?
  dataset_upload_mode <- "clear" # append, merge
  dataset_type <- "SERVER" # form dataset updates/uploads?

  # authentication
  upload_url <- stringr::str_glue(
    "{scto_auth$host}/datasets/{dataset_id}/upload?csrf_token={scto_auth$csrf_token}")

  # write temporary csv file
  csv_file <- fs::file_temp(ext = "csv")
  readr::write_csv(data, csv_file)

  # data upload
  upload_res <- httr::POST(
    upload_url,
    body = list(
      `dataset_exists` = as.numeric(dataset_exists),
      `dataset_id` = dataset_id,
      `dataset_title` = dataset_title,
      `dataset_upload_mode` = dataset_upload_mode,
      `dataset_type` = dataset_type,
      `dataset_file` = httr::upload_file(csv_file)))

  return(upload_res)
}
