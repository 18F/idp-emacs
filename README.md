# IDP Dev Tools for Emacs
Login.gov [identity-idp](https://github.com/18F/identity-idp) package for Emacs development.
  
## Installation
Aside from including the elisp file in your local config, the easiest way to install this package is to use [use-package](https://github.com/jwiegley/use-package) and [straight.el](https://github.com/radian-software/straight.el), like so:
  
```emacs-lisp
(use-package idp
  :ensure nil
  :straight '(idp :type git :host github :repo "18F/idp-emacs"))
```
