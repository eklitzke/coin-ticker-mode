# Emacs coin-ticker-mode

This is an Emacs minor-mode that displays the price of Bitcoin, Ethereum, and
other cryptocurrencies. Prices are fetched using
the [CoinMarketCap](https://coinmarketcap.com/) ticker API, which supports
nearly every cryptocurrency.

![Screenshot](https://github.com/eklitzke/coin-ticker-mode/blob/master/screenshot.png?raw=true)

## Installation

This package is currently awaiting approval to become part of MELPA: see
the [MELPA PR 4793](https://github.com/melpa/melpa/pull/4793).

## Configuration

Your Emacs config should have some code like this:

```elisp
(require 'coin-ticker)

;; Optional: Set the update interval, in seconds (default 300)
(setq coin-ticker-api-poll-interval 120)

;; Optional: Set the currency list (default is BTC and ETH)
(setq coin-ticker-syms '("BTC" "ETH" "LTC" "XPR"))

;; Optional: Just show prices (and no symbols)
(setq coin-ticker-show-syms nil)

;; Enable coin-ticker-mode
(coin-ticker-mode 1)
```
