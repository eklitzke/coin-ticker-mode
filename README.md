[![Melpa Status](http://melpa.org/packages/coin-ticker-badge.svg)](http://melpa.org/#/coin-ticker)

# Emacs coin-ticker-mode

This is an Emacs minor mode that can display a ticket with the price of Bitcoin,
Ethereum, and other cryptocurrencies. Prices are fetched using
the [CoinMarketCap](https://coinmarketcap.com/) ticker API, which supports
nearly every cryptocurrency.

![Screenshot](https://github.com/eklitzke/coin-ticker-mode/blob/master/screenshot.png?raw=true)

## Installation

You'll need Emacs 25 or later to use coin-ticker-mode.

The recommended way to get `coin-ticker` is as a package from the [MELPA][melpa]
repository. The version of `coin-ticker` there will always be up-to-date.

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

;; Optional: Set the price unit to use (default is "USD")
(setq coin-ticker-price-convert "EUR")

;; Optional: Set the symbol used to display prices (default is "$")
(setq coin-ticker-price-symbol "€")

;; Enable coin-ticker-mode
(coin-ticker-mode 1)
```

## License

This Emacs mode is free software, released under the GPLv3, like Emacs itself.

[melpa]: http://melpa.org
