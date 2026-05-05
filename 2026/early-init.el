(setq bedrock--initial-gc-threshold gc-cons-threshold)
(setq gc-cons-threshold 10000000)
(setq byte-compile-warnings '(not obsolete))
(setq warning-supress-log-types '((comp) (bytecomp)))
(setq native-comp-async-report-warnings-errors 'silent)

(setq inhibit-start-echo-area-message (user-login-name))

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://stable.melpa.org/packages/") t)

(setq frame-resize-pixelwise t)
(setq initial-frame-alist '((width . 70)
			    (height . 100)))
(tool-bar-mode -1)
