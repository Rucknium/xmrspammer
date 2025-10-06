
# monitor


compare.heights <- function(monerod.rpc.port, monero.wallet.rpc.port) {

  monerod.height <- xmr.rpc(paste0("http://127.0.0.1:", monerod.rpc.port, "/json_rpc"),
    method = "get_last_block_header")$result$block_header$height

  stopifnot(length(monerod.height) > 0)

  monero.wallet.rpc.height <- xmr.rpc(url.rpc = paste0("http://127.0.0.1:",
    monero.wallet.rpc.port, "/json_rpc"),
    method = "get_height",
    params = list())$result$height

  stopifnot(length(monero.wallet.rpc.height) > 0)

  c(monerod.height = monerod.height,
    monero.wallet.rpc.height = monero.wallet.rpc.height)

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
    pid.command <- system(paste0("ps -p ", pid, " -o command"), intern = TRUE)

    # Check if the process is still running and it was actually an
    # instance of monero-wallet-rpc
    if ( any(grepl("monero-wallet-rpc", pid.command)) ) {
      if (restart.alive.processes) {
        system(paste0("kill -9 ", pid))
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
      paste0("--daemon-address=", "127.0.0.1:", monerod.rpc.port),
      paste0("--log-file=", wallets[[id]][["wallet_dir"]], "/monero-wallet-rpc.log"),
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

  while( ! all(wallets.synced) ) {

    for (id in seq_along(wallets)) {

      heights <- compare.heights(monerod.rpc.port, wallets[[id]][["monero_wallet_rpc_port"]])

      cat(paste0(base::date(), "Wallet ", formatC(id, width = 3),
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

    log.text <- system(paste0("tail -n ", log.lines, " ", R.log.file), intern = TRUE)

    if (get_balance) {
      balance <- xmr.rpc(url.rpc = paste0("http://127.0.0.1:",
        x[["monero_wallet_rpc_port"]], "/json_rpc"),
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
        " wallet accounts:\n\t", sep = "")
      print(status[[id]]$balance.data$per_account.summary)
    }
    cat("\n")

  }

  return(invisible(NULL))

}
























