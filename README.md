# web-capf.el - Completions for web-related modes

[![GNU Emacs](https://img.shields.io/static/v1?logo=gnuemacs&logoColor=fafafa&label=Made%20for&message=GNU%20Emacs&color=7F5AB6&style=flat)](https://www.gnu.org/software/emacs/)

## What is this?

`web-capf` derives better completions for web-related modes, such as
`html-mode`, `css-mode` and `web-mode`.

In `web-mode`, it also supports sub parts in html like below:

* css parts inside `<style>` tags.
* javascript parts inside `<script>` tags or `<%`~`>` forms in .ejs file.
* php parts inside `<?php`~`>` forms in .php file.
* ruby parts inside `<%`~`>` forms in .erb file.

Note that the completions in script parts require the external capfs
suitable for those languages.

It works as a member of `completion-at-point-functions`,
that is, works with some modern completion frameworks,
[Corfu](https://github.com/minad/corfu),
[Company](https://github.com/company-mode/company-mode), and so on.

## Quickstart

```
(require 'web-capf)

;; use cape for script completion
(setq web-capf-javascript-fallback 'cape-keyword)
(setq web-capf-php-fallback 'cape-keyword)
(setq web-capf-ruby-fallback 'cape-keyword)

;; use company for script completion (needs also cape)
;;
;;(setq company-backends '((company-capf company-dabbrev-code)))
;;
;;(let ((capf (cape-company-to-capf
;;             (apply-partially #'company--multi-backend-adapter
;;                              '(company-dabbrev-code company-keywords)))))
;;  (setq web-capf-javascript-fallback capf)
;;  (setq web-capf-php-fallback capf)
;;  (setq web-capf-ruby-fallback capf))

(defun setup-web-capf ()
  (setq-local completion-at-point-functions
              (list (car completion-at-point-functions)
                    ;; also use cape.el
                    'cape-file 'web-capf 'cape-dabbrev)))

(add-hook 'web-mode-hook 'setup-web-capf)
;;(add-hook 'html-mode-hook 'setup-web-capf)
;;(add-hook 'css-mode-hook 'setup-web-capf)
```

## Configuration

### `web-capf-javascript-fallback`

Fallback capf for javascript part in `web-mode`.

Default is `nil`, then no completion in javascript part.

Note that the fallback configuration by adding some capfs, for example,
`cape-keyword` to `completion-at-point-functions` might not work due
to unmatched `major-mode`.

Setting this value will work well because it will make `web-capf` call
the fallback capf with masquerading `major-mode` to `javascript-mode`.

```
;; maybe not works
;; (setq completion-at-point-functions '(web-capf cape-keyword))

;; works well
(setq completion-at-point-functions '(web-capf))
(setq web-capf-javascript-fallback 'cape-keyword)
```

### `web-capf-php-fallback`

Fallback capf for php part in `web-mode`.

See `web-capf-javascript-fallback` for detail.

### `web-capf-ruby-fallback`

Fallback capf for ruby part in `web-mode`.

See `web-capf-javascript-fallback` for detail.

## Completion function

### `web-capf`

Set this function as a member of `completion-at-point-functions` for
`html-mode`, `css-mode` and `web-mode`.

That is all when use with `corfu-mode`, because it works as a frontend
of the standard completion framework in Emacs.

To use with `company-mode`, set company backend to `company-capf` and
set `completion-at-point-functions`.
