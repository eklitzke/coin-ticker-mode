# Emacs coin-ticker-mode

This is an Emacs minor-mode that displays the price of Bitcoin, Ethereum, and
other cryptocurrencies. Prices are fetched using
the [CoinMarketCap](https://coinmarketcap.com/) ticker API, which supports
nearly every cryptocurrency.

![Screenshot](https://github.com/eklitzke/coin-ticker-mode/blob/master/screenshot.png?raw=true)

## Installation

You'll need Emacs 25 or later to use coin-ticker-mode.

If you're adventurous, you can install directly from GitHub. This package is
currently awaiting approval to become part of MELPA: see
MELPA [PR #4793](https://github.com/melpa/melpa/pull/4793). If you're patient,
it should be approved there shortly, which will make installation easier.

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

## Contributing / License

This is free software, released under the GPLv3. Pull requests are welcome.
