# xmrspammer

<!-- badges: start -->

[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html) [![R-CMD-check](https://github.com/Rucknium/xmrspammer/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Rucknium/xmrspammer/actions/workflows/R-CMD-check.yaml)

<!-- badges: end -->

`xmrspammer` is an alpha project. Expect breaking changes in future versions.

## Installation

Install R. Linux users should install the `r-base` and `r-base-dev` system packages.

You can install the development version of `xmrspammer` from [GitHub](https://github.com/Rucknium/xmrspammer) if you have `git` and `curl` installed on your system with:

``` r
install.packages("remotes")
remotes::install_github("Rucknium/xmrspammer", upgrade = FALSE)
```

## ❗❗❗ WARNING: Wallets not secure

`xmrspammer` is intended to be used with valueless testnet coins. The security of the wallet creation and management processes have not been evaluated. If you use `xmrspammer` on mainnet, your coins could be stolen.

## Design

The Monero protocol has a few restrictions that make transaction spamming more difficult than, say, bitcoin.

First is the 16-output limit. The maximum number of outputs that a Monero transaction can have is 16. A single output cannot be as easily split into a very large number of outputs due to this restriction.

Second is the 10-block lock. Wallets must wait 10 confirmations to spend new outputs.

A third factor in stressnet testing is the need to fill the stressnet txpool to test node performance when the txpool is large. Transactions cannot be chained together in the txpool (i.e. child transactions cannot spend unconfirmed parent outputs). A wallet needs to have many outputs available to spend so that many of its unconfirmed transactions can wait in the txpool.

The design of `xmrspammer` works around these limitations. Wallets are set up to expand a single output into a "tree". At the end of the process, a wallet will have many "leaves" that contain a single output within an account. From there, the default spamming procedure iterates through each leaf, creating a self-spend transaction back to each leaf.

`xmrspammer` creates a user-specified number of wallets and boots a `monero-wallet-rpc` process for each wallet. A `monerod` Monero node process must be available on the local machine (TODO: allow remote nodes). For each wallet, `xmrspammer` spawns an R process that runs a spamming loop. The loop instructs each `monero-wallet-rpc` process to create and broadcast transactions as quickly as possible.

## Usage

`xmrspammer` has only been tested on Linux.

Users should view coins put into the spamming wallets as unrecoverable. Large amounts of coins should not be put into single spamming wallets because wallets with hundreds of thousands of transactions become slow to produce new transactions. At that point, the wallets should be abandoned and fresh wallets created. TODO: Suggestions for spamming wallet deposit amounts.

### Prerequisites

1.  You must have a Monero node running locally. It is best to enable the restricted RPC port because connections to the unrestricted RPC port experience performance problems when the txpool is large. You can start your Monero node with the standard testnet restricted RPC port by adding `--rpc-restricted-bind-port=28089` the flag in your `monerod` startup command. If you prefer not to enable the restricted RPC port, the *unrestricted* RPC port for testnet at 28081 is available by default to connections from within your local machine. For a smooth connection experience, also add `--rpc-max-connections=1000`, `--rpc-max-connections-per-public-ip=1000` and `--rpc-max-connections-per-private-ip=1000` as startup flags.

2.  You should have a wallet with some testnet XMR. You will send coins from this wallet to the spamming wallets.

3.  You must create a new empty working directory. Place the correct version of `monero-wallet-rpc` into the working directory.

Starting with a small-scale test wallet can help familiarize users with the process and discover bugs early.

### Small-scale test

Start a new R instance in a terminal in the working directory. Use `screen` or another similar program to prevent losing the instance. Once you have installed `xmrspammer`, load the package into the workspace:

``` r
library(xmrspammer)
```

You must specify the `monerod` RPC port and the number of spamming wallets to be created. In this example we will assume that you have the node's restricted RPC port available at 28089. We will create one wallet for this test.

``` r
wallets <- gen.wallets(monerod.rpc.port = 28089, n.wallets = 1)
```

A single `monero-wallet-rpc` process should have launched to manage the single wallet we created. Check that the process is alive:

``` r
wallets[[1]]$monero_wallet_rpc_process$is_alive()
```

The console should print `TRUE`.

Next, we will fund the spamming wallet. `fund.wallets()` is a convenience function that will print the exact command to input into the `monero-wallet-cli` of the funding wallet. Input the `amount` argument to specify the total amount of XMR you will send to the spamming wallet(s).

``` r
fund.wallets(wallets, amount = 30)
```

R will print a statement like

``` txt
transfer priority <address> 30 subtractfeefrom=all
```

Input the statement into `monero-wallet-cli` and confirm broadcast of the transaction. You will have to wait 10 blocks or about 20 minutes for the outputs of the transaction to be spendable for the next step.

Now we will create the account leaves of the wallet. This test wallet will use three levels of 15-output transactions. The process will create `15^3  = 3375` leaf accounts. Fee priority will be set to 4 for each level. There are four standard fee tiers: 1, 2, 3, and 4. When the txpool is full, a low-fee transaction could take a long time to confirm, slowing down this step in the process. We will choose fee priority 4 for all tree levels so that the accounts are built as quickly as possible.

``` r
wallets <- prep.leaves.wallets(wallets, n.outputs = 15,
  n.tree.levels = 3, fee.priority = c(4, 4, 4))
```

These transactions should take about 60 minutes (30 blocks) to become spendable.

Finally, start the spamming. The next line will initiate an infinite loop. Input `ctrl + c` to interrupt the spamming loop if desired.

``` r
spam.1in.2out.wallets(wallets)
```

You can check the status of the spamming wallets by starting a new R session in a different terminal window (or a different `screen` session). You must start the R session in the working directory where you started the original R session. It should have some files and subdirectories by now. Remember to load the package into the workspace:

``` r
library(xmrspammer)
```

Now load the wallet data into the workspace:

``` r
wallets <- readRDS("spam_wallets_data.rds")
```

Get some status information about the wallet:

``` r
wallets.status(wallets, get_balance = TRUE)
```

Ignore the message about the RPC process being dead. You may see log messages about insufficient funds for creating new transactions. When the txpool is large, there will be many transactions in the queue in front of yours, so you may have all of your accounts with transactions waiting in the txpool.

### Large-scale use

TODO

### Revive wallets

TODO
