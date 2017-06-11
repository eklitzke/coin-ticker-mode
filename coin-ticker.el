;;; coin-ticker.el --- Show a cryptocurrency price ticker

;; Copyright (C) 2017  Evan Klitzke

;; Author: Evan Klitzke <evan@eklitzke.org>
;; URL: https://github.com/eklitzke/coin-ticker-mode
;; Version: 0.9
;; Package-Requires: ((request "0.2.0") (emacs "25"))
;; Keywords: news

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Provides a ticker of cryptocurrency prices (Bitcoin, Ethereum, etc.) using
;; the coinmarketcap.io API

;;; Code:

(require 'request)
(require 'json)

(defgroup coin-ticker nil
  "coin-ticker extension"
  :group 'comms
  :prefix "coincap-ticker-")

(defconst coin-ticker-url "https://api.coinmarketcap.com/v1/ticker/")

(defcustom coin-ticker-api-poll-interval 300
  "Default interval to poll to the coinmarketcap api (in seconds)"
  :type 'number
  :group 'coin-ticker)

;; hash table that holds prices
(defvar coin-ticker-syms '("BTC" "ETH")
  "Coins to show")

(defvar coin-ticker-show-syms t
  "If non-nil, symbols will be shown alongside prices")

(defvar coin-ticker-timer nil
  "Coin API poll timer")

(defvar coin-ticker-mode-line ""
  "Displayed on mode-line")

;; risky bidnis
(put 'coin-ticker-mode-line 'risky-local-variable t)

(defun coin-ticker-start ()
  (unless coin-ticker-timer
    (setq coin-ticker-timer
          (run-at-time "0 sec"
                       coin-ticker-api-poll-interval
                       #'coin-ticker-fetch))
    (coin-ticker-fetch)))

(defun coin-ticker-stop()
  (when coin-ticker-timer
    (cancel-timer coin-ticker-timer)
    (setq coin-ticker-timer nil)
    (if (boundp 'mode-line-modes)
        (delete '(t coin-ticker-mode-line) mode-line-modes))))

(defvar coin-ticker-prices (make-hash-table :test 'equal))

(defun coin-ticker-price-fmt (sym price)
  (if coin-ticker-show-syms
      (format "%s $%s" sym price)
    (format "$%s" price)))

(defun coin-ticker-modeline-update ()
  (setq coin-ticker-mode-line
        (format "[%s]" (string-join
                        (cl-loop for sym in coin-ticker-syms
                                 collect
                                 (coin-ticker-price-fmt sym (gethash sym coin-ticker-prices))) " "))))

(defun coin-ticker-fetch ()
  (request
   coin-ticker-url
   :params '(("limit" . "10"))
   :parser 'json-read
   :success (cl-function
             (lambda (&key data &allow-other-keys)
               (cl-loop for tick across data
                        do (let ((sym (alist-get 'symbol tick))
                                 (price (alist-get 'price_usd tick)))
                             (puthash sym price coin-ticker-prices)))
               (coin-ticker-modeline-update)))))

(define-minor-mode coin-ticker-mode
  "Minor mode to show cryptocurrency prices"
  :init-value nil
  :global t
  :lighter coin-ticker-mode-line
  (if coin-ticker-mode
      (progn (coin-ticker-start))
    (coin-ticker-stop)))

(provide 'coin-ticker)
;;; coin-ticker.el ends here
