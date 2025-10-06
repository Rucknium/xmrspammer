




spam.1in.2out <- function(wallet, fee.priority = 1,
  delay.between.each.tx = 0) {

  monero_wallet_rpc_port <- wallet$monero_wallet_rpc_port
  leaf_accounts <- data.table::copy(wallet$leaf_accounts)
  # Use copy() because it is a data.table and copy() forces pass-by-value
  wallet_dir <- wallet$wallet_dir

  priority <- fee.priority

  handle <- RCurl::getCurlHandle()

  row.iter <- 1

  # options(width = 200)

  while (TRUE) {

    sweep.result <- xmr.rpc(url.rpc = paste0("http://127.0.0.1:", monero_wallet_rpc_port, "/json_rpc"),
      method = "sweep_all", params = list(address = leaf_accounts[row.iter, base_address],
        account_index = leaf_accounts[row.iter, as.numeric(account_index)], priority = priority),
      keep.trying.rpc = TRUE, handle = handle)

    if (length(sweep.result$error) > 0) {
      cat(paste0(base::date(),
        " Account: ", leaf_accounts[row.iter, label],
        " Error: ", sweep.result$error, "\n"),
        file = paste0(wallet_dir, "/R.log"), append = TRUE)

    } else {
      cat(paste0(base::date(),
        " Account: ", leaf_accounts[row.iter, label],
        " Amount: ", formatC(unlist(sweep.result$result$amount_list) / 1e+12, digits = 8, format = "fg", flag = "#"),
        " Fee: ", formatC(unlist(sweep.result$result$fee_list) / 1e+12, digits = 8, format = "fg", flag = "#"),
        " tx_hash: ", unlist(sweep.result$result$tx_hash_list), "\n"),
        file = paste0(wallet_dir, "/R.log"), append = TRUE)
    }

    if (length(sweep.result$result$spent_key_images_list[[1]]$key_images) > 1) {
      # If multiple outputs are being spent, wait longer
      Sys.sleep(1)

      if (row.iter %% 20 == 0) {
        cat(base::date(), " Sleep 20 seconds\n", file = paste0(wallet_dir, "/R.log"), append = TRUE)
        Sys.sleep(20)
        # Give time to propagate blocks and/or txs to peers
      }

    }

    row.iter <- row.iter + 1

    if (row.iter > nrow(leaf_accounts)) {
      row.iter <- 1
      # stop("end of accounts")
    }

    if (row.iter %% 2000 == 0) {
      cat(base::date(), "Sleep 30 seconds\n", file = paste0(wallet_dir, "/R.log"), append = TRUE)
      Sys.sleep(30)
      # Give time to propagate blocks and/or txs to peers
      save_wallet(monero_wallet_rpc_port, handle = handle)
      # Save wallet to storage
    }

    Sys.sleep(delay.between.each.tx)

  }

  return(invisible(NULL))

}




#' Spam 1in/2out transactions
#'
#' @param wallets wallets
#' @param threads threads
#' @param fee.priority fee.priority
#' @param delay.between.each.tx delay.between.each.tx
#'
#' @returns
#' 1
#' @export
#'
#' @examples
#' 1
spam.1in.2out.wallets <- function(wallets, threads = NA, fee.priority = 1,
  delay.between.each.tx = 0) {

  if (is.na(threads)) { threads <- length(wallets) }
  future::plan(future::multisession, workers = threads)

  on.exit( future::plan(future::sequential) )
  # Terminate threads on exit (keyboard interrupt)

  wallets.trimmed <- vector("list", length(wallets))

  for (id in seq_along(wallets)) {
    wallets.trimmed[[id]][["monero_wallet_rpc_port"]] <-
      wallets[[id]][["monero_wallet_rpc_port"]]
    wallets.trimmed[[id]][["leaf_accounts"]] <-
      data.table::copy(wallets[[id]][["leaf_accounts"]])
    # Use copy() because it is a data.table and copy() forces pass-by-value
    # to the multisession future
    wallets.trimmed[[id]][["wallet_dir"]] <-
      wallets[[id]][["wallet_dir"]]
  }
  # Don't want to send whole wallets object into the future_lapply(). It
  # has the `processx` of the `monero-wallet-rpc` processes

  future.apply::future_lapply(wallets.trimmed,
    FUN = function(wallet) {
    spam.1in.2out(wallet, fee.priority = fee.priority,
      delay.between.each.tx = delay.between.each.tx)
  }, future.packages = "data.table")
  # future.packages = "data.table" because the leaf_accounts data.table
  # object must be recognized as a data.table instead of just a data.frame

  return(invisible(NULL))

}




