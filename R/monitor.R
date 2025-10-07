
# monitor


compare.heights <- function(monerod.rpc.port, monero.wallet.rpc.port,
  monerod.handle, wallet.handle) {

  monerod.height <- xmr.rpc(paste0(parse.url.port(monerod.rpc.port), "/json_rpc"),
    method = "get_last_block_header", handle = monerod.handle)$result$block_header$height

  stopifnot(length(monerod.height) > 0)

  monero.wallet.rpc.height <- xmr.rpc(url.rpc = paste0(
    parse.url.port(monero.wallet.rpc.port), "/json_rpc"),
    method = "get_height",
    params = list(),
    handle = wallet.handle)$result["height"]

  stopifnot(length(monero.wallet.rpc.height) > 0)

  result <- as.numeric(c(monerod.height, monero.wallet.rpc.height))

  names(result) <- c("monerod.height", "monero.wallet.rpc.height")

  result

}









#' Revive wallets processes
#'
#' @param monerod.rpc.port monerod.rpc.port
#' @param wallets.data.file wallets.data.file
#' @param restart.alive.processes restart.alive.processes
#' @param print.json print.json
#' @param cleanup.process.monero_wallet_rpc cleanup.process.monero_wallet_rpc
#'
#' @returns
#' 1
#' @export
#'
#' @examples
#' 1
revive.wallets <- function(monerod.rpc.port,
  wallets.data.file = "spam_wallets_data.rds",
  restart.alive.processes = FALSE,
  print.json = FALSE,
  cleanup.process.monero_wallet_rpc = FALSE) {
  # if restart.alive.processes, then kill process before restarting it

  wallets <- readRDS(wallets.data.file)

  monero_wallet_rpc.binary <- ifelse(.Platform$OS.type == "windows",
    "monero-wallet-rpc.exe", "./monero-wallet-rpc")

  for (id in seq_along(wallets)) {

    pid <- wallets[[id]]$monero_wallet_rpc_pid
    pid.handle <- tryCatch(ps::ps_handle(pid), error = function(e) NULL)

    # Check if the process is still running and it was actually an
    # instance of monero-wallet-rpc
    if ( length(pid.handle) > 0 && grepl("monero-wallet-rpc", ps::ps_name(pid.handle)) ) {
      if (restart.alive.processes) {
        ps::ps_kill(pid.handle)
        Sys.sleep(5)
        # Sleep so that the process has time to be killed
      } else {
        next
        # If the process is already running and we do not
        # want to restart it, go to next wallet
      }
    }

    wallets[[id]][["monero_wallet_rpc_port"]] <- parallelly::freePort()
    # Get a new port

    monero.wallet.rpc.startup.flags <- c(
      "--testnet",
      paste0("--rpc-bind-port=", wallets[[id]][["monero_wallet_rpc_port"]]),
      paste0("--wallet-file=", wallets[[id]][["wallet_dir"]], "/spam_wallet"),
      paste0("--daemon-address=", parse.url.port(monerod.rpc.port, rm.http.prefix = TRUE) ),
      paste0("--log-file=", wallets[[id]][["wallet_dir"]], "/monero-wallet-rpc.log"),
      paste0("--rpc-max-connections=", "1000"),
      paste0("--rpc-max-connections-per-public-ip=", "1000"),
      paste0("--rpc-max-connections-per-private-ip=", "1000"),
      "--password=password",
      "--trusted-daemon", "--disable-rpc-ban", "--disable-rpc-login"
    )
    # Separate args into elements in a vector. No spaces. Use "=" instead of space.

    monero.wallet.rpc.process <- processx::process$new(monero_wallet_rpc.binary,
      monero.wallet.rpc.startup.flags, cleanup = cleanup.process.monero_wallet_rpc)

    wallets[[id]][["monero_wallet_rpc_process"]] <-
      monero.wallet.rpc.process

    wallets[[id]][["monero_wallet_rpc_pid"]] <- monero.wallet.rpc.process$get_pid()

    wallets[[id]][["json"]]$monero_wallet_rpc_port <- wallets[[id]][["monero_wallet_rpc_port"]]
    wallets[[id]][["json"]]$monero_wallet_rpc_pid <- wallets[[id]][["monero_wallet_rpc_pid"]]

  }

  cat("\nWaiting for wallets to finish syncing...\n\n")
  Sys.sleep(10)

  wallets.synced <- rep(FALSE, length(wallets))
  wallets.handles <- replicate(length(wallets), RCurl::getCurlHandle())
  # May need "replicate" instead of "rep" because this causes handle creation
  monerod.handle <- RCurl::getCurlHandle()

  while( ! all(wallets.synced) ) {

    for (id in seq_along(wallets)) {

      heights <- compare.heights(monerod.rpc.port, wallets[[id]][["monero_wallet_rpc_port"]],
        monerod.handle = monerod.handle, wallet.handle = wallets.handles[[id]])

      cat(paste0(base::date(), " Wallet ", formatC(id, width = 3),
        " sync height: ",
        heights["monero.wallet.rpc.height"], "/", heights["monerod.height"], "\n"))

      save_wallet(wallets[[id]][["monero_wallet_rpc_port"]])

      wallets.synced[id] <- ifelse(abs(diff(heights)) <= 3, TRUE, FALSE)
      # Tolerance of 3 blocks
    }

    if ( ! all(wallets.synced)) {
      Sys.sleep(60)
    }

  }

  all.json <- RJSONIO::toJSON(lapply(wallets, FUN = function(x) {x[["json"]]}),
    pretty = TRUE)

  cat(all.json, "\n", file = gsub("[.]rds$", ".json", wallets.data.file))

  if (print.json) {
    cat(all.json, "\n")
  }

  saveRDS(wallets, file = wallets.data.file)

  return(wallets)

}








#' Check wallets status
#'
#' @param wallets wallets
#' @param get_balance get_balance
#' @param log.lines log.lines
#'
#' @returns
#' 1
#' @export
#'
#' @examples
#' 1
wallets.status <- function(wallets, get_balance = FALSE, log.lines = 2) {

  status <- lapply(wallets, function(x) {

    alive <- x$monero_wallet_rpc_process$is_alive()

    R.log.file <- paste0(x[["wallet_dir"]], "/R.log")
    total.log.n.lines <- LaF::determine_nlines(R.log.file)
    log.text <- LaF::get_lines(R.log.file,
      (total.log.n.lines - log.lines + 1):total.log.n.lines)

    if (get_balance) {
      balance <- xmr.rpc(url.rpc = paste0(
        parse.url.port(x[["monero_wallet_rpc_port"]]), "/json_rpc"),
        method = "get_balance",
        params = list(account_index = 0L, all_accounts = TRUE))$result

      stopifnot(length(balance$balance) > 0)

      total.balance <- as.numeric(balance$balance) / 1e+12
      # Convert from piconeros

      per_subaddress <- sapply(balance$per_subaddress,
        function(z) { as.numeric(z["balance"]) / 1e+12 })

      n.accounts <- length(per_subaddress)

      per_account.summary <- summary(per_subaddress)

      balance.data <- list(total.balance = total.balance, n.accounts = n.accounts,
        per_account.summary = per_account.summary)

    } else {
      balance.data <- NULL
    }

    list(alive = alive, log.text = log.text, balance.data = balance.data)

  })

  for (id in seq_along(wallets)) {

    cat("Wallet: ", wallets[[id]]$wallet_dir, "\n", sep = "")
    cat("\tmonero-wallet-rpc process with pid ", wallets[[id]]$monero_wallet_rpc_pid,
      " listening at port ", wallets[[id]]$monero_wallet_rpc_port,
      " is ", ifelse(status[[id]]$alive, "alive.", "dead."), "\n", sep = "")
    cat("\tTail of R process log file:\n")
    cat(paste0("\t", status[[id]]$log.text), sep = "\n")

    if (get_balance) {
      cat("\tTotal wallet balance: ",
        status[[id]]$balance.data$total.balance, "\n", sep = "")
      cat("Summary stats of balances of ", status[[id]]$balance.data$n.accounts,
        " wallet accounts:\n", sep = "")
      print(status[[id]]$balance.data$per_account.summary)
    }
    cat("\n")

  }

  return(invisible(NULL))

}
























