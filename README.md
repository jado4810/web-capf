# web-capf.el - Completions for web-related modes

[![GNU Emacs](https://img.shields.io/static/v1?logo=gnuemacs&logoColor=fafafa&label=Made%20for&message=GNU%20Emacs&color=7F5AB6&style=flat)](https://www.gnu.org/software/emacs/)

## What is this?

`web-capf` derives better completions for web-related modes, such as
`html-mode`, `css-mode` and `web-mode`.

In `web-mode`, it also supports sub parts in html like below:

* css parts inside `<style>` tags
* javascript parts inside `<script>` tags or `<%`~`>` forms in .ejs file
* php parts inside `<?php`~`>` forms in .php file
* ruby parts inside `<%`~`>` forms in .erb file

Note that the completions in script parts require the external capfs
suitable for those languages.

It works as a member of `completion-at-point-functions`,
that is, works with some modern completion frameworks,
[Corfu](https://github.com/minad/corfu),
[Company](https://github.com/company-mode/company-mode), and so on.

## Quickstart

### Using corfu-mode and cape to complete codes

```Emacs Lisp
;;; use corfu-mode and cape
(require 'corfu)
(require 'orderless)
(require 'cape)
(require 'cape-keyword)
(require 'web-capf)

;; setup corfu-mode
(setq corfu-auto t)
(setq corfu-auto-prefix 1)
(setq corfu-on-exact-match nil)
(setq corfu-preselect-first nil)
(setq corfu-cycle t)
(global-corfu-mode 1)

(add-hook 'minibuffer-setup-hook
  (lambda ()
    (when (where-is-internal 'completion-at-point (list (current-local-map)))
      (setq-local corfu-auto nil)
      (corfu-mode 1))))

(add-hook 'corfu-mode-hook
  (lambda ()
    (setq completion-styles '(orderless))
    (setq completion-category-defaults nil)
    (setq completion-category-overrides nil)
    (setq tab-always-indent 'complete)
    (setq tab-first-completion 'eol)
    (setq-local orderless-style-dispatchers
                '((lambda (pattern index total)
                    (and (eq index 0) 'orderless-flex))))))

;; setup capfs for web-mode; set web-capf and some capfs derived by cape
(defun setup-capf ()
  (setq-local completion-at-point-functions
              (list (car completion-at-point-functions)
                    'cape-file 'web-capf 'cape-keyword 'cape-dabbrev)))

(add-hook 'text-mode-hook 'setup-capf)
(add-hook 'prog-mode-hook 'setup-capf)
(add-hook 'web-mode-hook 'setup-capf)
(add-to-list 'completion-at-point-functions 'cape-file t)
(add-to-list 'completion-at-point-functions 'web-capf t)
(add-to-list 'completion-at-point-functions 'cape-keyword t)
(add-to-list 'completion-at-point-functions 'cape-dabbrev t)

;; use cape for script completion
(setq web-capf-javascript-fallback 'cape-keyword)
(setq web-capf-php-fallback 'cape-keyword)
(setq web-capf-ruby-fallback 'cape-keyword)
```

### Using company to complete codes (also needs cape)

```Emacs Lisp
;;; use company (also needs cape)
(require 'company)
(require 'cape)
(require 'web-capf)

;; setup company
(setq company-idle-delay 0)
(setq company-minimum-prefix-length 2)
(setq company-selection-wrap-around t)
(setq completion-ignore-case t)
(setq company-dabbrev-downcase nil)
(setq company-transformers '(company-sort-by-backend-importance))

;; setup company-backends; set company-dabbrev-code and company-capf,
;; to refer to standard capfs
(setq company-backends '((company-capf company-dabbrev-code)))
(setq completion-at-point-functions '(web-capf))

;; use company for script completion
(let ((capf (cape-company-to-capf
             (apply-partially #'company--multi-backend-adapter
                              '(company-dabbrev-code company-keywords)))))
  (setq web-capf-javascript-fallback capf)
  (setq web-capf-php-fallback capf)
  (setq web-capf-ruby-fallback capf))
```

## Configuration

### `web-capf-javascript-fallback`

Fallback capf for javascript part in `web-mode`.

Default is `nil`, then no completion in javascript part.

Note that the fallback configuration by adding some capfs, for example,
`cape-keyword` to `completion-at-point-functions` might not work due
to unmatched `major-mode`.

Setting this value works well because it makes `web-capf` call the
fallback capf with masquerading `major-mode` to `javascript-mode`.

```Emacs Lisp
;;; use corfu-mode and cape
;; might not work inside javascript part in web-mode
;; (cape-keyword works only in native javascript-mode)
(setq completion-at-point-functions '(web-capf cape-keyword))

;; works well in web-mode by making web-capf call cape-keyword
(setq web-capf-javascript-fallback 'cape-keyword)
```

```Emacs Lisp
;;; use company (also needs cape)
;; might not work inside javascript part in web-mode
;; (company-dabbrev-code works only in native javascript-mode)
(setq company-backends '((company-capf company-dabbrev-code)))
(setq completion-at-point-functions '(web-capf))

;; works well in web-mode by making web-capf call cape-keyword
(setq web-capf-javascript-fallback
      (cape-company-to-capf
       (apply-partially #'company--multi-backend-adapter
                        '(company-dabbrev-code company-keywords))))
```

### `web-capf-php-fallback`

Fallback capf for php part in `web-mode`.

See [`web-capf-javascript-fallback`](#web-capf-javascript-fallback)
for detail.

### `web-capf-ruby-fallback`

Fallback capf for ruby part in `web-mode`.

See [`web-capf-javascript-fallback`](#web-capf-javascript-fallback)
for detail.

## Completion function

### `web-capf`

Set this function as a member of `completion-at-point-functions` for
`html-mode`, `css-mode` and `web-mode`.

That is all when use with `corfu-mode`, because it works as a frontend
of the standard completion framework in Emacs.

```Emacs Lisp
;;; use corfu-mode and cape
(setq completion-at-point-functions
      '(cape-file web-capf cape-keyword cape-dabbrev))
```

To use with `company-mode`, set company backend to `company-capf` and
set `completion-at-point-functions`.

```Emacs Lisp
;;; use company
(setq company-backends '((company-capf company-dabbrev-code)))
(setq completion-at-point-functions '(web-capf))
```
