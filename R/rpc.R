

parse.url.port <- function(x, rm.http.prefix = FALSE) {
  if (is.numeric(x)) {
    # Assume local URL
    return(paste0("http://127.0.0.1:", x))
  }
  pattern <- "^http[s]?://[0-9]{1,3}[.][0-9]{1,3}[.][0-9]{1,3}[.][0-9]{1,3}:[0-9]+$"
  if (grepl(pattern, x)) {
    if (rm.http.prefix) {
      x <- gsub("^http[s]?://", "", x)
    }
    return(x)
  }
  stop("Input must be a numeric port like 99999 or a URL with port like http://999.999.999.999:99999")
}


# Modified from TownforgeR::tf_rpc_curl function
xmr.rpc <- function(
    url.rpc = "http://127.0.0.1:18081/json_rpc",
  method = "",
  params = list(),
  userpwd = "",
  num.as.string = TRUE,
  nonce.as.string = FALSE,
  keep.trying.rpc = FALSE,
  handle = RCurl::getCurlHandle(),
  ...
){

  json.ret <- RJSONIO::toJSON(
    list(
      jsonrpc = "2.0",
      id = "0",
      method = method,
      params = params
    ), digits = 50
  )

  rcp.ret <- 	tryCatch(RCurl::postForm(url.rpc,
    .opts = list(
      userpwd = userpwd,
      postfields = json.ret,
      httpheader = c('Content-Type' = 'application/json', Accept = 'application/json')
      # https://stackoverflow.com/questions/19267261/timeout-while-reading-csv-file-from-url-in-r
    ),
    curl = handle
  ), error = function(e) {NULL})

  if (keep.trying.rpc && length(rcp.ret) == 0) {
    while (length(rcp.ret) == 0) {
      rcp.ret <- 	tryCatch(RCurl::postForm(url.rpc,
        .opts = list(
          userpwd = userpwd,
          postfields = json.ret,
          httpheader = c('Content-Type' = 'application/json', Accept = 'application/json')
          # https://stackoverflow.com/questions/19267261/timeout-while-reading-csv-file-from-url-in-r
        )
      ), error = function(e) {NULL})
    }
  }

  if (is.null(rcp.ret)) {
    stop(paste0("Cannot connect to ", url.rpc, "  Is the process running?"))
  }

  if (num.as.string) {
    rcp.ret <- gsub("(: )([-0123456789.]+)([,\n\r])", "\\1\"\\2\"\\3", rcp.ret )
  }

  if (nonce.as.string & ! num.as.string) {
    rcp.ret <- gsub("(\"nonce\": )([-0123456789.]+)([,\n\r])", "\\1\"\\2\"\\3", rcp.ret )
  }

  RJSONIO::fromJSON(rcp.ret, asText = TRUE) # , simplify = FALSE
}


get_accounts <- function(wallet_rpc_port, handle = RCurl::getCurlHandle() ) {
  xmr.rpc(paste0(parse.url.port(wallet_rpc_port), "/json_rpc"),
    method = "get_accounts", handle = handle)
}


restore_deterministic_wallet <- function(wallet_rpc_port, wallet_id, seed, restore_height) {

  filename <- paste0(formatC(wallet_id, width = 3, flag = "0"), "_",
    paste0(seed[1:2], collapse = "_"))

  result <- xmr.rpc(paste0(parse.url.port(wallet_rpc_port), "/json_rpc"),
    method = "restore_deterministic_wallet",
    params = list(
      filename = "spam_wallet",
      password = "password",
      # Must have password be "password", literally, because a restart of
      # monero-wallet-rpc doesn't like empty passwords.
      seed = paste0(seed, collapse = " "),
      restore_height = restore_height,
      language = "English",
      seed_offset = "",
      autosave_current = TRUE
    )
  )

  result
}


save_wallet <- function(wallet_rpc_port, handle = RCurl::getCurlHandle()) {
  # Save wallet file to storage
  xmr.rpc(paste0(parse.url.port(wallet_rpc_port), "/json_rpc"),
    method = "store", params = list(), handle = handle)
}










