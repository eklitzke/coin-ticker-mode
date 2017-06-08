# Emacs coin-ticker-mode

This is an Emacs minor-mode that displays the price of Bitcoin (and other
cryptocurrencies such as Ethereum) on the mode-line. Prices are fetched using
the [CoinMarketCap](https://coinmarketcap.com/) ticker API.

![Screenshot](https://github.com/eklitzke/coin-ticker-mode/blob/master/screenshot.png?raw=true)

## Installation

Your Emacs config should have some code like this:

```elisp
(require 'coin-ticker)

;; Optional: You can setup the update interval, in seconds (default 300)
(setq coin-ticker-api-poll-interval 120)

;; Optional: Set the currencies to use (default is BTC and ETH)
(setq coin-ticker-syms '("BTC" "ETH" "LTC" "XPR"))

;; Optional: just show prices (and no symbols)
(setq coin-ticker-show-syms nil)

;; Enable coin-ticker-mode
(coin-ticker-mode 1)
```
