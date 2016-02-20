
authBaseURL <- "https://utslogin.nlm.nih.gov"
authEndpoint <- "/cas/v1/tickets"

umls_env <- new.env(parent = emptyenv())

#' Get a TGT token from the UMLS API.
#'
#' TGT tokens are used to generate service tickets that allow for requests to be ran against the UMLS API.
#'
#' @param name The UMLS username.
#' @param pass The UMLS password.
#'
#' @return Returns nothing, adding the TGT token to the environment.
get_TGT <- function(name = NULL, pass = NULL) {
    TGT <- umls_env$TGT
    if (is.null(TGT) && !is.null(name) && !is.null(pass)) {
        params <- list(username = name, password = pass)
        authURL <- paste0(authBaseURL, authEndpoint)
        r <- POST(url = authURL, body = params, encode = "form")
        if (status_code(r) == 201) {
            htmlResponse <- read_html(r)
            TGT <- html_attr(html_nodes(htmlResponse, "form"), "action")
            set_TGT(TGT)
            message("Authenticated.")
        } else {
            stop("Error authenticating.")
        }
    } else if (is.null(TGT) && (is.null(name) | !is.null(pass))) {
        stop("No username/password provided.")
    }

    TGT
}

#' @rdname get_TGT
set_TGT <- function(value) {
    umls_env$TGT <- value
}

#' @rdname get_TGT
reset_TGT <- function() {
    set_TGT(NULL)
}

#' Get a service ticket.
#'
#' @param TGT The TGT token for the current session.
#'
#' @return Returns nothing, adding the service token to the environment.
get_service_ticket <- function(TGT) {
    r <- POST(url = TGT, body = list(service = "http://umlsks.nlm.nih.gov"), encode = "form")
    htmlResponse <- read_html(r)
    ticketValue <- html_text(html_nodes(htmlResponse, "body"))
    set_service_ticket(ticketValue)
}

#' @rdname get_service_ticket
set_service_ticket <- function(value) {
    umls_env$service_ticket <- value
}

#' Authenticate against UMLS.
#'
#' @export
auth_UMLS <- function() {
    creds <- UMLS_creds()
    invisible(get_TGT(creds$user, creds$pass))
}

#' Get or set UMLS credentials.
#'
#' @export
UMLS_creds <- function(force = FALSE){
  user <- Sys.getenv('UMLS_USER')
  pass <- Sys.getenv('UMLS_PASS')
  if (!identical(user, "") && !identical(pass, "") && !force) return(list(user = user, pass = pass))

  if (!interactive()) {
    stop("Please set env vars UMLS_USER, UMLS_PASS to your UMLS username/password respectively.",
      call. = FALSE)
  }

  message("Couldn't find env vars UMLS_USER, UMLS_PASS. See ?UMLS_creds for more details.")
  message("Please enter your username and press enter:")
  user <- readline(": ")
  message("Please enter your password and press enter:")
  pass <- readline(": ")
  if (identical(user, "") | identical(pass, "")) {
    stop("User/password entry failed", call. = FALSE)
  }

  message("Updating env vars.")
  Sys.setenv(UMLS_USER = user, UMLS_PASS = pass)

  list(user = user, pass = pass)
  }
