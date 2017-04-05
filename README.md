# waf-mode
Waf integration for Emacs
Waf Documentation: https://github.com/waf-project/waf

## How to enable:

```
(use-package waf-mode
  :ensure t
  :pin melpa
  :init
  (setq waf-mode-keymap-prefix (kbd "C-c b"))  ;; Or any other prefix - you prefer. By defaul it will be "C-c ^".
  (add-hook 'python-mode-hook #'waf-conditionally-enable)
  (add-hook 'c++-mode-hook #'waf-conditionally-enable)
  (add-hook 'c-mode-common-hook #'waf-conditionally-enable))
```

## Default key bindings:

1. "C-c ^" - prefix
2. "<prefix> b" - build
3. "<prefix> c" - clean
4. "<prefix> C" - configure
5. "<prefix> r" - re-build
6. "<prefix> R" - re-configure
7. "<prefix> B" - re-build all

