helm-xref
============

[![Melpa](https://melpa.org/packages/helm-xref-badge.svg)](http://melpa.milkbox.net/#/helm-xref)

Helm interface for [xref](https://www.gnu.org/software/emacs/manual/html_node/emacs/Xref.html)

![](https://raw.githubusercontent.com/brotzeitmacher/helm-xref/master/helm-xref.png)

## Setup

The package can be installed from MELPA.

    (require 'helm-xref)
    (if (< emacs-major-version 27)
        (setq xref-show-xrefs-function 'helm-xref-show-xrefs)
      (setq xref-show-xrefs-function 'helm-xref-show-xrefs-27))

