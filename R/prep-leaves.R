




gen.account.tree.level <- function(wallet_rpc_port, transfer.tree, n.outputs,
  tree_level, priority, handle = RCurl::getCurlHandle() ) {
  # TODO: Have a condition for if not all txs confirm, i.e. if waiting for more
  # than one hour, then just exit the function and continue.

  all_accounts <- get_accounts(wallet_rpc_port, handle = handle)$result
  # Do this outside of loop because the call is slow

  for (i in unique(transfer.tree[[tree_level]]$account_index)) {

    account_balance <- as.numeric(all_accounts$subaddress_accounts[[as.numeric(i) + 1]]["unlocked_balance"])
    # i + 1 because accounts are indexed from 0

    if (account_balance == 0) {
      # Just in case account balance is zero, skip this one
      next
    }

    # print(account_balance / 1e+12)
    send_to_each_address <- floor(account_balance / n.outputs)
    leftover_piconeros <- account_balance %% n.outputs
    # Put remainder piconeros in the first address.
    send_to_each_address <- rep(send_to_each_address, n.outputs)
    send_to_each_address[1] <- send_to_each_address[1] + leftover_piconeros
    to_addresses <- transfer.tree[[tree_level]][account_index == i, address]
    destinations <- list()
    for (j in seq_along(to_addresses)) {
      destinations[[j]] <- list(amount = send_to_each_address[j], address = to_addresses[j])
    }
    params = list(destinations = destinations, account_index = as.numeric(i), priority = priority,
      subtract_fee_from_outputs = seq_along(to_addresses) - 1L)
    transfer.result <- xmr.rpc(url.rpc = paste0("http://127.0.0.1:", wallet_rpc_port, "/json_rpc"),
      method = "transfer", params = params, handle = handle)
    # str(transfer.result, 1)
    # print(transfer.result$result$tx_hash)
  }


  wait.for.confirmations <- ifelse(tree_level == length(transfer.tree), FALSE, TRUE)
  # If on the last tree level, don't wait

  while (wait.for.confirmations) {

    Sys.sleep(120)
    all_accounts <- get_accounts(wallet_rpc_port, handle = handle)$result
    unlocked_account_balance.positive <- TRUE

    for (i in unique(transfer.tree[[tree_level + 1L]]$account_index)) {
      account_balance <- as.numeric(all_accounts$subaddress_accounts[[as.numeric(i) + 1]]["unlocked_balance"])
      unlocked_account_balance.positive <- unlocked_account_balance.positive & account_balance > 0
    }

    if (unlocked_account_balance.positive) {
      wait.for.confirmations <- FALSE
    }

  }

  save_wallet(wallet_rpc_port)

  return(invisible(NULL))

}




prep.leaves <- function(wallet, wallet_rpc_port, n.outputs = 16, n.tree.levels = 4,
  fee.priority = c(3, 3, 3, 0), handle = RCurl::getCurlHandle()) {

  wallet_rpc_port <- wallet[["monero_wallet_rpc_port"]]
  wallet_dir <- wallet[["wallet_dir"]]

  stopifnot( length(fee.priority) == n.tree.levels & all(fee.priority %in% 0:4) )
  fee.priority <- as.integer(fee.priority)

  if ( ! n.outputs %in% 2:15) {
    stop("n.outputs must be an integer between 2 and 15")
  }

  n.final.accounts <- n.outputs^n.tree.levels
  final.accounts <- paste0(formatC(n.tree.levels, width = 2, flag = "0"), "-",
    formatC(seq_len(n.final.accounts), width = 6, flag = "0"))

  for (i in 1:n.tree.levels) {
    n.final.accounts <- n.final.accounts / n.outputs
    final.accounts <- cbind(rep(paste0(formatC(n.tree.levels - i, width = 2, flag = "0"), "-",
      formatC(seq_len(n.final.accounts), width = 6, flag = "0")), each = n.outputs^i), final.accounts)
  }


  transfer.tree <- list()

  unique.tree.accounts <- sort(unique(c(final.accounts)))

  for (i in 1:n.tree.levels) {
    transfer.tree[[i]] <- unique(final.accounts[, i:(i + 1)])
    colnames(transfer.tree[[i]]) <- c("from", "to")
    transfer.tree[[i]] <- data.table::as.data.table(transfer.tree[[i]])
  }


  # MUST DO THIS WHEN STARTING NEW WALLET:
  xmr.rpc(url.rpc = paste0("http://127.0.0.1:", wallet_rpc_port, "/json_rpc"),
    method = "label_account",
    params = list(account_index = 0L, label = unique.tree.accounts[1]),
    handle = handle)


  all_accounts <- get_accounts(wallet_rpc_port, handle = handle)$result

  stopifnot(all_accounts$subaddress_accounts[[1]]["label"] == "00-000001")
  # Make sure that the primary account got labeled "00-000001"


  existing.accounts <- vector("character", length(all_accounts$subaddress_accounts))

  for (i in seq_along(all_accounts$subaddress_accounts)) {
    existing.accounts[i] <- all_accounts$subaddress_accounts[[i]]["label"]
  }


  for (i in setdiff(unique.tree.accounts, existing.accounts)) {
    new_account <- xmr.rpc(url.rpc = paste0("http://127.0.0.1:", wallet_rpc_port, "/json_rpc"),
      method = "create_account", params = list(label = i), handle = handle)
    # Sys.sleep(0.01)
  }



  all_accounts <- get_accounts(wallet_rpc_port, handle = handle)$result

  accounts <- vector("list", length(all_accounts$subaddress_accounts))

  for (i in seq_along(all_accounts$subaddress_accounts)) {
    x <- all_accounts$subaddress_accounts[[i]]
    accounts[[i]] <- data.table(account_index = x["account_index"],
      label = x["label"], address = x["base_address"])
  }

  accounts <- data.table::rbindlist(accounts)


  for (i in seq_along(transfer.tree)) {
    transfer.tree[[i]] <- merge(transfer.tree[[i]], accounts[, .(label, account_index)],
      by.x = "from", by.y = "label")
    # transfer.tree[[1]][, label := NULL]
    transfer.tree[[i]] <- merge(transfer.tree[[i]], accounts[, .(label, address)],
      by.x = "to", by.y = "label")
  }


  for (tree_level in seq_len(n.tree.levels)) {

    gen.account.tree.level(wallet_rpc_port, transfer.tree, n.outputs = n.outputs,
      tree_level = tree_level, priority = fee.priority[tree_level], handle = handle)

    cat(paste0(base::date(), " Completed tree_level ", tree_level,
      " with fee priority ", fee.priority[tree_level],
      " to wallet_rpc_port ", wallet_rpc_port, "\n"),
      file = paste0(wallet_dir, "/R.log"), append = TRUE)

  }


  all_accounts <- get_accounts(wallet_rpc_port, handle = handle)$result

  leaf.accounts <- lapply(all_accounts$subaddress_accounts, FUN = function(x) {
    data.table(account_index = x[["account_index"]], base_address = x[["base_address"]], label = x[["label"]])
  })

  leaf.accounts <- data.table::rbindlist(leaf.accounts)

  leaf.accounts <- leaf.accounts[substr(label, 1, 2) ==
      formatC(n.tree.levels, width = 2, flag = "0"), ]


  wait.for.confirmations <- TRUE

  while (wait.for.confirmations) {

    Sys.sleep(120)
    all_accounts <- get_accounts(wallet_rpc_port, handle = handle)$result
    unlocked_account_balance.positive <- TRUE

    for (i in unique(leaf.accounts$account_index)) {
      account_balance <- as.numeric(all_accounts$subaddress_accounts[[as.numeric(i) + 1]]["unlocked_balance"])
      unlocked_account_balance.positive <- unlocked_account_balance.positive & account_balance > 0
    }

    if (unlocked_account_balance.positive) {
      wait.for.confirmations <- FALSE
    }

  }


  cat(paste0(base::date(),
    " Completed preparation of ", n.tree.levels,
    " levels with ", n.outputs,
    " outputs each, producing ", n.outputs^n.tree.levels, " leaf accounts ",
    " to wallet_rpc_port ", wallet_rpc_port, "\n"),
    file = paste0(wallet_dir, "/R.log"), append = TRUE)


  return(leaf.accounts)
  # leaf.accounts is needed for spamming at the leaf level

}





#' Prepare Wallet Account Tree Leaves
#'
#' @param wallets wallets
#' @param threads threads
#' @param n.outputs n.outputs
#' @param n.tree.levels n.tree.levels
#' @param fee.priority fee.priority
#' @param wallets.data.file wallets.data.file
#'
#' @returns
#' 1
#' @export
#'
#' @examples
#' 1
prep.leaves.wallets <- function(wallets, threads = NA, n.outputs = 16, n.tree.levels = 4,
  fee.priority = c(3, 3, 3, 0), wallets.data.file = "spam_wallets_data.rds") {

  if (is.na(threads)) { threads <- length(wallets) }
  future::plan(future::multisession, workers = threads)

  wallets.trimmed <- vector("list", length(wallets))

  for (id in seq_along(wallets)) {
    wallets.trimmed[[id]][["monero_wallet_rpc_port"]] <-
      wallets[[id]][["monero_wallet_rpc_port"]]
    wallets.trimmed[[id]][["wallet_dir"]] <-
      wallets[[id]][["wallet_dir"]]
  }
  # Don't want to send whole wallets object into the future_lapply(). It
  # has the `processx` of the `monero-wallet-rpc` processes

  leaf.accounts.wallets <- future.apply::future_lapply(wallets.trimmed,
    FUN = function(wallet) {
    prep.leaves(wallet, n.outputs = n.outputs,
      n.tree.levels = n.tree.levels, fee.priority = fee.priority)
  })

  future::plan(future::sequential)
  # Terminate threads

  for (id in seq_along(leaf.accounts.wallets)) {
    wallets[[id]][["leaf_accounts"]] <- leaf.accounts.wallets[[id]]
  }

  saveRDS(wallets, file = wallets.data.file)

  wallets

}









