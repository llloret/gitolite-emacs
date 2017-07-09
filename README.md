# About

This is an emacs major mode that provides some features for
editing [gitolite](http://gitolite.com/gitolite/index.html) configuration files.

# Installation

This package is available on [Melpa](https://melpa.org/#/). With it, you should
be able find the package using `M-x list-packages`.

Alternatively, you can
use [use-package](https://github.com/jwiegley/use-package) to easily add it to
your local configuration:

    (use-package gl-conf-mode
      :ensure t
      :defer t)

# Features

The current features include:

- context-sensitive help for gitolite.conf files: `C-c C-h`
- move to next repository definition: `C-c C-n`
- move to previous repository definition: `C-c C-p`
- go to the include file on the line where the cursor is: `C-c C-v`
- open a navigation window with all the repositories (hyperlink enabled): `C-c C-l`
- open a navigaton window with all the group definitions (hyperlink enabled): `C-c C-g`
- mark the current repository and body: `C-c C-m`

It supports basic syntax based font colours, so it will highlight interesting
parts of the configuration file:

- comments
- repo definitions
- rules
- refexes
- groups
- ...

For the context sensitive help it can detect different positions, and will offer
help on that topic:

- repo line
- include line
- permissions (R/RW/RWC/...)
- refexes (branches, ...)
- user or group permissions
- groups
- anything else (offer generic gitolite.conf help)

The help uses the main gitolite web documentation, linking directly into it with
a browser.

If the emacs w3m module is available in the system, it will be used to open the
help inside emacs, otherwise, the emacs configured external browser will be
launched (based on the Emacs variable `browse-url-browser-function`).
