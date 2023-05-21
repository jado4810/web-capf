;;; web-capf.el --- Completions for web-related modes -*- lexical-binding: t -*-

;; Copyright (C) 2022  Free Software Foundation, Inc.

;; Author: Shun-ichi Tahara <jado@flowernet.jp>
;; Maintainer: Shun-ichi Tahara <jado@flowernet.jp>
;; Created: 2022
;; Version: 0.2
;; Package-Requires: ((emacs "27.1"))
;; Homepage: https://github.com/jado4810/web-capf

;; This file is part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
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

;; This package provides the `web-capf' completion function,
;; which delives better completion for web-related modes,
;; such as `html-mode', `css-mode' and `web-mode'.

;;; Code:

(eval-when-compile
  (require 'subr-x))

(defvar web-capf-compl-types
  '((html-mode . html)
    (css-mode . css)
    (web-mode . complex-web))
  "Alist of web-related major-modes and completion types.
Type complex-web works only with `web-mode'.
For the other-modes like `nxhtml-mode', need more implementations.")

(defvar web-capf-engines-and-modes
  '((erb . ruby)
    (ejs . javascript))
  "Alist of `web-mode' engines and `web-capf' completion modes,
for sub block part of `web-mode'.")

(defvar web-capf-javascript-fallback nil
  "Fallback capf for javascript part in `web-mode'.
Note that fallback in `completion-at-point-functions' might not work
if set, for example, `cape-keyword', because `major-mode' is different.
This calls capf with masquerading `major-mode' to `javascript-mode'.")

(defvar web-capf-php-fallback nil
  "Fallback capf for php part in `web-mode'.
Note that fallback in `completion-at-point-functions' might not work
if set, for example, `cape-keyword', because `major-mode' is different.
This calls capf with masquerading `major-mode' to `php-mode'.")

(defvar web-capf-ruby-fallback nil
  "Fallback capf for ruby part in `web-mode'.
Note that fallback in `completion-at-point-functions' might not work
if set, for example, `cape-keyword', because `major-mode' is different.
This calls capf with masquerading `major-mode' to `ruby-mode'.")

(defconst web-capf-html-decls-and-attrs
  '((cdata) (doctype "html"))
  "Alist of html5 declaration types and attribute names,
or cdata sections, which start with \"<!\".")

(defconst web-capf-html-insts
  '("xml")
  "List of html processing instructions for xml,
which start with \"<?\".")

(defconst web-capf-html-noclose-tags
  '(area base br col embed hr img input link meta source)
  "List of html5 tags without close tags.")

(defconst web-capf-html-autoclose-tags
  '((caption (tbody tfoot th thead tr) . (table))
    (dd (dd dt) . (dl))
    (dt (dd dt) . (dl))
    (li (li) . (ol ul))
    (optgroup (optgroup) . (select))
    (option (opt optgroup) . (select))
    (p (address article aside blockquote dir div dl fieldset footer
        form h1 h2 h3 h4 h5 h6 header hgroup hr main menu nav ol p pre
        section table ul))
    (rp (rp rt) . (ruby))
    (rt (rp rt) . (ruby))
    (tbody (tbody tfoot) . (table))
    (td (td th) . (tr))
    (tfoot (tbody) . (table))
    (th (td th) . (tr))
    (thead (tbody tfoot))
    (tr (tr) . (table tbody tfoot thead)))
  "List of html5 tags to be closed automatically.")

(defconst web-capf-html-tag-hierarchies
  '((a (web-capf--parent-rule
        web-capf--section web-capf--block
        abbr area audio b bdi bdo br canvas cite code data datalist
        del dfn em i img input ins kbd map mark math meter noscript
        object output picture progress q ruby s samp script small span
        strong sub sup svg template time u var video wbr))
    (abbr web-capf--inline (web-capf--ancestor map area))
    (address web-capf--block web-capf--inline
             (web-capf--ancestor map area))
    (article web-capf--section web-capf--block web-capf--inline
             (web-capf--ancestor map area))
    (aside web-capf--section web-capf--block web-capf--inline
           (web-capf--ancestor map area))
    (audio (web-capf--parent-rule
            web-capf--section web-capf--block
            a abbr b bdi bdo br button canvas cite code data datalist
            del dfn em embed i iframe img input ins kbd label map mark
            math meter noscript object output picture progress q ruby
            s samp script select small span strong sub sup svg
            template textarea time u var wbr)
           source track)
    (b web-capf--inline (web-capf--ancestor map area))
    (blockquote web-capf--section web-capf--block web-capf--inline
                (web-capf--ancestor map area))
    (body web-capf--section web-capf--block web-capf--inline
          link main)
    (button abbr audio b bdi bdo br canvas cite code data datalist del
            dfn em i img input ins kbd map mark math meter noscript
            object output picture progress q ruby s samp script small
            span strong sub sup svg template time u var video wbr
            (web-capf--ancestor map area))
    (canvas (web-capf--parent-rule
             web-capf--section web-capf--block web-capf--inline area))
    (caption web-capf--section web-capf--inline
             blockquote details dialog div dl fieldset figure form hr
             ol p pre ul
             (web-capf--ancestor map area))
    (cite web-capf--inline (web-capf--ancestor map area))
    (code web-capf--inline (web-capf--ancestor map area))
    (colgroup col template)
    (data web-capf--inline (web-capf--ancestor map area))
    (datalist web-capf--inline option (web-capf--ancestor map area))
    (dd web-capf--section web-capf--block web-capf--inline
        (web-capf--ancestor map area))
    (del (web-capf--parent-rule
          web-capf--section web-capf--block web-capf--inline area))
    (details web-capf--section web-capf--block web-capf--inline
             summary (web-capf--ancestor map area))
    (dfn (web-capf--norecurse web-capf--inline)
         (web-capf--ancestor map area))
    (dialog web-capf--section web-capf--block web-capf--inline
            legend (web-capf--ancestor map area))
    (div web-capf--section web-capf--block web-capf--inline
         dd dt main (web-capf--ancestor map area))
    (dl div dd dt script template)
    (dt web-capf-block web-capf--inline
        address (web-capf--ancestor map area))
    (em web-capf--inline (web-capf--ancestor map area))
    (fieldset web-capf--section web-capf--block web-capf--inline
              legend (web-capf--ancestor map area))
    (figcaption web-capf--section web-capf--block web-capf--inline
                (web-capf--ancestor map area))
    (figure web-capf--section web-capf--block web-capf--inline
            figcaption (web-capf--ancestor map area))
    (footer web-capf-block web-capf--inline
            address article aside h1 h2 h3 h4 h5 h6 nav section
            (web-capf--ancestor map area))
    (form web-capf--section web-capf--inline
          blockquote details dialog div dl fieldset figure hr ol p pre
          table ul
          (web-capf--ancestor map area))
    (h1 web-capf--inline (web-capf--ancestor map area))
    (h2 web-capf--inline (web-capf--ancestor map area))
    (h3 web-capf--inline (web-capf--ancestor map area))
    (h4 web-capf--inline (web-capf--ancestor map area))
    (h5 web-capf--inline (web-capf--ancestor map area))
    (h6 web-capf--inline (web-capf--ancestor map area))
    (head base link meta noscript script style template title)
    (header web-capf-block web-capf--inline
            address article aside h1 h2 h3 h4 h5 h6 nav section
            (web-capf--ancestor map area))
    (html body head)
    (i web-capf--inline (web-capf--ancestor map area))
    (ins (web-capf--parent-rule
          web-capf--section web-capf--block web-capf--inline area))
    (kbd web-capf--inline (web-capf--ancestor map area))
    (label (web-capf--norecurse web-capf--inline)
           (web-capf--ancestor map area))
    (legend web-capf--inline (web-capf--ancestor map area))
    (li web-capf--section web-capf--block web-capf--inline
        (web-capf--ancestor map area))
    (main web-capf--section web-capf--block web-capf--inline
          (web-capf--ancestor map area))
    (map (web-capf--parent-rule
          web-capf--section web-capf--block web-capf--inline)
         area)
    (mark web-capf--inline (web-capf--ancestor map area))
    (meter (web-capf--norecurse web-capf-inline)
           (web-capf--ancestor map area))
    (nav web-capf--section web-capf--block web-capf--inline
         (web-capf--ancestor map area))
    (noscript (web-capf--parent-rule
               web-capf--section web-capf--block web-capf--inline
               area link meta style))
    (object web-capf--section web-capf--block web-capf--inline
            (web-capf--ancestor map area))
    (ol li script template)
    (optgroup option script templete)
    (output web-capf--inline (web-capf--ancestor map area))
    (p web-capf--inline (web-capf--ancestor map area))
    (picture img source)
    (pre web-capf--inline (web-capf--ancestor map area))
    (progress (web-capf--norecurse web-capf-inline)
              (web-capf--ancestor map area))
    (q web-capf--inline (web-capf--ancestor map area))
    (rb web-capf--inline (web-capf--ancestor map area))
    (rp web-capf--inline (web-capf--ancestor map area))
    (rt web-capf--inline (web-capf--ancestor map area))
    (rtc web-capf--inline rt (web-capf--ancestor map area))
    (ruby web-capf--inline rb rp rt rtc (web-capf--ancestor map area))
    (s web-capf--inline (web-capf--ancestor map area))
    (samp web-capf--inline (web-capf--ancestor map area))
    (section web-capf--section web-capf--block web-capf--inline
             (web-capf--ancestor map area))
    (select optgroup option script template)
    (small web-capf--inline (web-capf--ancestor map area))
    (span web-capf--inline (web-capf--ancestor map area))
    (strong web-capf--inline (web-capf--ancestor map area))
    (sub web-capf--inline (web-capf--ancestor map area))
    (summary web-capf--inline
             h1 h2 h3 h4 h5 h6 (web-capf--ancestor map area))
    (sup web-capf--inline (web-capf--ancestor map area))
    (table caption colgroup script tbody template tfoot thead tr)
    (tbody script template tr)
    (td web-capf--section web-capf--block web-capf--inline
        (web-capf--ancestor map area))
    (template web-capf--section web-capf--block web-capf--inline
              base caption col colgroup dd dt figcaption legend li
              link meta optgroup option param rb rp rt rtc source
              style tbody td tfoot th thead title tr track
              (web-capf--ancestor map area))
    (tfoot script template tr)
    (th web-capf-block web-capf--inline
        address (web-capf--ancestor map area))
    (thead script template tr)
    (time web-capf--inline (web-capf--ancestor map area))
    (tr script template td th)
    (u web-capf--inline (web-capf--ancestor map area))
    (ul li script template)
    (var web-capf--inline (web-capf--ancestor map area))
    (video (web-capf--parent-rule
            web-capf--section web-capf--block
            a abbr area b bdi bdo br button canvas cite code data
            datalist del dfn em embed i iframe img input ins kbd label
            map mark math meter noscript object output picture
            progress q ruby s samp script select small span strong sub
            sup svg template textarea time u var wbr)
           source track))
  "Alist of html5 tags hierarchy rules.")

(defconst web-capf-html-section-tags
  '(address article aside footer h1 h2 h3 h4 h5 h6 header nav section)
  "List of html5 tags available where section tags expected.")

(defconst web-capf-html-block-tags
  '(blockquote details dialog div dl fieldset figure form hr ol p pre
    table ul)
  "List of html5 tags available where block-like tags expected.")

(defconst web-capf-html-inline-tags
  '(a abbr audio b bdi bdo br button canvas cite code data datalist
    del dfn em embed i iframe img input ins kbd label map mark math
    meter noscript object output picture progress q ruby s samp script
    select small span strong sub sup svg template textarea time u var
    video wbr)
  "List of html5 tags available where inline-like tags expected.")

(defconst web-capf-html-tags-and-attrs
  '((a
     "download" "href" "hreflang" "ping" "referrerpolicy" "rel"
     "target" "type")
    (abbr)
    (address)
    (area
     "alt" "coords" "download" "href" "hreflang" "ping"
     "referrerpolicy" "rel" "shape" "target")
    (article)
    (aside)
    (audio
     "autoplay" "controls" "controlslist" "crossorigin"
     "disableremoteplayback" "loop" "muted" "preload" "src")
    (b)
    (base "href" "target")
    (bdi) (bdo "dir")
    (blockquote "cite")
    (body)
    (br)
    (button
     "disabled" "form" "formaction" "formenctype" "formmethod"
     "formnovalidate" "formtarget" "name" "type" "value")
    (canvas "height" "width")
    (caption)
    (cite)
    (code)
    (col "span") (colgroup "span")
    (data "value") (datalist)
    (dd)
    (del "cite" "datetime")
    (details "open")
    (dfn)
    (dialog "open")
    (div)
    (dl) (dt)
    (em)
    (embed "height" "src" "type" "width")
    (fieldset "disabled" "form" "name")
    (figcaption) (figure)
    (footer)
    (form
     "accept-charset" "action" "autocomplete" "enctype" "method"
     "name" "novalidate" "rel" "target")
    (h1) (h2) (h3) (h4) (h5) (h6)
    (head)
    (header)
    (hr)
    (html "xmlns")
    (i)
    (iframe
     "allow" "allowfullscreen" "csp" "height" "loading" "name"
     "referrerpolicy" "sandbox" "src" "srcdoc" "width")
    (img
     "alt" "crossorigin" "decoding" "fetchpolicy" "height" "ismap"
     "loading" "referrerpolicy" "sizes" "src" "srcset" "width"
     "usemap")
    (input
     "accept" "alt" "autocomplete" "capture" "checked" "dirname"
     "disabled" "form" "formaction" "formenctype" "formmethod"
     "formnovalidate" "formtarget" "height" "list" "max" "maxlength"
     "min" "minlength" "multiple" "name" "pattern" "placeholder"
     "readonly" "required" "size" "src" "step" "type" "value" "width")
    (ins "cite" "datetime")
    (kbd)
    (label "for")
    (legend)
    (li "value")
    (link
     "as" "crossorigin" "disabled" "fetchpolicy" "href" "hreflang"
     "imagesizes" "imagesrcset" "integrity" "media" "prefetch"
     "referrerpolicy" "rel" "sizes" "type")
    (main)
    (map "name")
    (mark)
    (menu)
    (meta "charset" "content" "http-equiv")
    (meter "form" "high" "low" "max" "min" "optimum" "value")
    (nav)
    (noscript)
    (object "data" "form" "height" "name" "type" "usemap" "width")
    (ol "reversed" "start" "type")
    (optgroup "disabled" "label")
    (option "disabled" "label" "selected" "value")
    (output "for" "form" "name")
    (p)
    (picture)
    (portal "referrerpolicy" "src")
    (pre)
    (progress "max" "value")
    (q "cite")
    (rp) (rt) (ruby)
    (s)
    (samp)
    (script
     "async" "crossorigin" "defer" "integrity" "nomodule"
     "referrerpolicy" "src" "type")
    (section)
    (select
     "autocomplete" "disabled" "form" "multiple" "name" "required"
     "size")
    (slot "name")
    (small)
    (source "media" "sizes" "src" "srcset" "type")
    (span)
    (strong)
    (style "media" "type")
    (sub)
    (summary)
    (sup)
    (table)
    (tbody)
    (td "colspan" "headers" "rowspan")
    (template)
    (textarea
     "autocomplete" "cols" "disabled" "form" "maxlength" "minlength"
     "name" "placeholder" "readonly" "required" "rows" "wrap")
    (tfoot)
    (th "abbr" "colspan" "headers" "rowspan" "scope")
    (thead)
    (time "datetime")
    (title)
    (tr)
    (track "default" "kind" "label" "srclang")
    (u)
    (ul)
    (var)
    (video
     "autoplay" "autopictureinpicture" "controls" "controlslist"
     "crossorigin" "disablepictureinpicture" "disableremoteplayback"
     "height" "loop" "muted" "playsinline" "poster" "preload" "src"
     "width")
    (wbr))
  "Alist of html5 tags and attribute names.")

(defconst web-capf-html-global-attrs
  '("accesskey" "autocapitalize" "autofocus" "blocking" "class"
    "contenteditable" "data" "dir" "draggable" "enterkeyhint"
    "exportparts" "hidden" "id" "inert" "inputmode" "is" "itemid"
    "itemprop" "itemref" "itemscope" "itemtype" "lang" "nonce" "part"
    "role" "slot" "spellcheck" "style" "tabindex" "title" "translate")
  "List of html5 global attribute names.")

(defconst web-capf-html-attr-vals
  '((as
     "audio" "document" "embed" "fetch" "font" "image" "object"
     "script" "style" "track" "video" "worker")
    (autocapitalize
     "characters" "none" "off" "on" "sentences" "words")
    (autocomplete
     (form "off" "on")
     (textarea "off" "on")
     (t
      "additional-name" "address-level1" "address-level2"
      "address-level3" "address-level4" "address-line1"
      "address-line2" "address-line3" "bday" "bday-day" "bday-month"
      "bday-year" "cc-additional-name" "cc-csc" "cc-exp"
      "cc-exp-month" "cc-exp-year" "cc-family-name" "cc-given-name"
      "cc-name" "cc-number" "cc-type" "country" "country-name"
      "current-password" "email" "family-name" "given-name"
      "honorific-prefix" "honorific-suffix" "impp" "language" "name"
      "new-password" "nickname" "off" "on" "one-time-code"
      "organization" "organization-title" "photo" "postal-code" "sex"
      "street-address" "tel" "tel-area-code" "tel-country-code"
      "tel-extension" "tel-local" "tel-national" "transaction-amount"
      "transaction-currency" "url" "username"))
    (blocking "render")
    (charset "utf-8")
    (contenteditable "false" "true")
    (controlslist "nodownload" "nofullscreen" "noremoteplayback")
    (crossorigin "anonymous" "use-credentials")
    (decoding "async" "auto" "sync")
    (dir
     (bdo "ltr" "rtl")
     (t "auto" "ltr" "rtl"))
    (draggable "false" "true")
    (enctype
     "application/x-www-form-urlencoded" "multipart/form-data"
     "text/plain")
    (fetchpolicy "auto" "high" "low")
    (formenctype enctype)
    (formmethod method)
    (formtarget target)
    (http-equiv
     "content-security-policy" "content-type" "default-style" "name"
     "refresh" "x-ua-compatible")
    (kind "captions" "chapters" "descriptions" "metadata" "subtitles")
    (loading "eager" "lazy")
    (method "dialog" "get" "post")
    (shape "circle" "default" "poly" "rect")
    (preload "auto" "metadata" "none")
    (referrerpolicy
     "no-referrer" "no-referrer-when-downgrade" "origin"
     "origin-when-cross-origin" "same-origin" "strict-origin"
     "strict-origin-when-cross-origin" "unsafe-url")
    (rel
     (a
      "alternate" "author" "bookmark" "external" "help" "license"
      "next" "nofollow" "noopener" "noreferrer" "opener" "prev"
      "search" "tag")
     (area
      "alternate" "author" "bookmark" "external" "help" "license" "me"
      "next" "nofollow" "noopener" "noreferrer" "opener" "prev"
      "search" "tag")
     (form
      "external" "help" "license" "next" "nofollow" "noopener"
      "noreferrer" "opener" "prev" "search")
     (link
      "alternate" "author" "canonical" "dns-prefetch" "help" "icon"
      "license" "manifest" "me" "modulepreload" "next" "pingback"
      "preconnect" "prefetch" "preload" "prerender" "prev" "search"
      "shortlink" "stylesheet"))
    (sandbox
     "allow-downloads-without-user-activation" "allow-downloads"
     "allow-forms" "allow-modals" "allow-orientation-lock"
     "allow-pointer-lock" "allow-popups"
     "allow-popups-to-escape-sandbox" "allow-presentation"
     "allow-same-origin" "allow-scripts"
     "allow-storage-access-by-user-activation" "allow-top-navigation"
     "allow-top-navigation-by-user-activation")
    (scope "col" "colgroup" "row" "rowgroup")
    (spellcheck
     (textarea "default" "false" "true")
     (t "false" "true"))
    (target "_blank" "_parent" "_self" "_top")
    (translate "no" "yes")
    (type
     (button "button" "submit" "reset")
     (input
      "button" "checkbox" "color" "date" "datetime-local" "email"
      "file" "hidden" "image" "month" "number" "password" "radio"
      "range" "reset" "search" "submit" "tel" "text" "time" "url"
      "week")
     (ol "1" "a" "A" "i" "I")
     (script "module" "text/javascript")
     (style "text/css"))
    (wrap "hard" "off" "soft")
    (xmlns "http://www.w3.org/1999/xhtml"))
  "Alist of html5 attribute names and values.")

(defconst web-capf-css-at-keywords
  '("charset" "color-profile" "counter-style" "font-face"
    "font-feature-values" "import" "keyframes" "layer" "media"
    "namespace" "page" "property" "supports")
  "List of css3 at-keywords, which start with \"@\" like media queries.")

(defconst web-capf-css-pseudo-elems
  '("after" "backdrop" "before" "cue" "cue-region"
    "file-selector-button" "first-letter" "first-line" "grammar-error"
    "marker" "part(" "placeholder" "-webkit-scrollbar"
    "-webkit-scrollbar-thumb" "selection" "slotted(" "spelling-error"
    "target-text")
  "List of css3 pseudo elements, which start with \"::\" in selectors.")

(defconst web-capf-css-pseudo-classes
  '("active" "any-link" "autofill" "blank" "checked" "default"
    "defined" "dir(" "disabled" "empty" "enabled" "first"
    "first-child" "first-of-type" "focus" "focus-visible"
    "focus-within" "fullscreen" "has(" "host" "host(" "host-context("
    "hover" "in-range" "indeterminate" "invalid" "is(" "lang("
    "last-child" "last-of-type" "left" "link" "modal" "not("
    "nth-child(" "nth-last-child(" "nth-last-of-type(" "nth-of-type("
    "only-child" "only-of-type" "optional" "out-of-range" "paused"
    "picture-in-picture" "placeholder-shown" "playing" "read-only"
    "read-write" "required" "right" "root" "scope" "target"
    "user-invalid" "-moz-ui-invalid" "user-valid" "-moz-ui-valid"
    "valid" "visited" "where(")
  "List of css3 pseudo classes, which start with \":\" in selectors.")

(defconst web-capf-css-sel-func-args
  '((dir "ltr" "rtl")
    (has web-capf--sels)
    (host web-capf--sels)
    (host-context web-capf--sels)
    (is web-capf--sels)
    (not web-capf--sels)
    (nth-child class--nth)
    (nth-col class--nth)
    (nth-last-child class--nth)
    (nth-last-col class--nth)
    (nth-last-of-type class--nth)
    (nth-of-type class--nth)
    (where web-capf--sels))
  "Alist of css3 pseudo selector function names and arguments.")

(defconst web-capf-css-props-and-vals
  '((accent-color class--color "auto")
    (align-content
     class--flex-align-container class--flex-align-baseline)
    (align-items class--flex-align-content)
    (align-self class--flex-align-content "auto")
    (align-tracks align-content)
    (all)
    (animation
     animation-direction animation-fill-mode animation-iteration-count
     animation-name animation-play-state animation-timing-function)
    (animation-composition "add" "accumulate" "replace")
    (animation-delay class--math-function)
    (animation-direction
     "alternate" "alternate-reverse" "normal" "reverse")
    (animation-duration class--math-function)
    (animation-fill-mode "backwards" "both" "forwards" "none")
    (animation-iteration-count class--math-function "infinite")
    (animation-name "none")
    (animation-play-state "paused" "running")
    (animation-timeline "auto" "none")
    (animation-timing-function class--easing-function)
    (appearance "auto" "menulist-button" "none" "textfield")
    (aspect-ratio class--math-function "auto")
    (backdrop-filter class--filter-function "none" "url(")
    (backface-visibility "hidden" "visible")
    (background
     background-attachment background-clip background-color
     background-image background-origin background-position
     background-repeat background-size)
    (background-attachment "fixed" "local" "scroll")
    (background-blend-mode class--blend-mode)
    (background-clip class--visual-box "text")
    (background-color class--color)
    (background-image class--image "none")
    (background-origin class--visual-box)
    (background-position class--position)
    (background-position-x class--position-x)
    (background-position-y class--position-y)
    (background-repeat
     "no-repeat" "repeat" "repeat-x" "repeat-y" "round" "space")
    (background-size class--math-function "auto" "contain" "cover")
    (block-size width)
    (border border-color border-style border-width)
    (border-block
     border-block-color border-block-style border-block-width)
    (border-block-color class--color)
    (border-block-end
     border-block-end-color border-block-end-style
     border-block-end-width)
    (border-block-end-color class--color)
    (border-block-end-style border-style)
    (border-block-end-width border-width)
    (border-block-start
     border-block-start-color border-block-start-style
     border-block-start-width)
    (border-block-start-color class--color)
    (border-block-start-style border-style)
    (border-block-start-width border-width)
    (border-block-style border-style)
    (border-block-width border-width)
    (border-bottom
     border-bottom-color border-bottom-style border-bottom-width)
    (border-bottom-color class--color)
    (border-bottom-left-radius class--math-function)
    (border-bottom-right-radius class--math-function)
    (border-bottom-style border-style)
    (border-bottom-width border-width)
    (border-collapse "collapse" "separate")
    (border-color class--color)
    (border-end-end-radius class--math-function)
    (border-end-start-radius class--math-function)
    (border-image
     border-image-outset border-image-repeat border-image-slice
     border-image-source border-image-width)
    (border-image-outset class--math-function)
    (border-image-repeat "repeat" "round" "space" "stretch")
    (border-image-slice class--math-function "fill")
    (border-image-source class--image "none")
    (border-image-width class--math-function "auto")
    (border-inline
     border-inline-color border-inline-style border-inline-width)
    (border-inline-color class--color)
    (border-inline-end
     border-inline-end-color border-inline-end-style
     border-inline-end-width)
    (border-inline-end-color class--color)
    (border-inline-end-style border-style)
    (border-inline-end-width border-width)
    (border-inline-start
     border-inline-start-color border-inline-start-style
     border-inline-start-width)
    (border-inline-start-color class--color)
    (border-inline-start-style border-style)
    (border-inline-start-width border-width)
    (border-inline-style border-style)
    (border-inline-width border-width)
    (border-left
     border-left-color border-left-style border-left-width)
    (border-left-color class--color)
    (border-left-style border-style)
    (border-left-width border-width)
    (border-radius class--math-function)
    (border-right
     border-right-color border-right-style border-right-width)
    (border-right-color class--color)
    (border-right-style border-style)
    (border-right-width border-width)
    (border-spacing class--math-function)
    (border-start-end-radius class--math-function)
    (border-start-start-radius class--math-function)
    (border-style class--line-style "hidden")
    (border-top border-top-color border-top-style border-top-width)
    (border-top-color class--color)
    (border-top-left-radius class--math-function)
    (border-top-right-radius class--math-function)
    (border-top-style border-style)
    (border-top-width border-width)
    (border-width class--line-width)
    (bottom class--math-function "auto")
    (box-decoration-break "clone" "slice")
    (box-shadow class--color class--math-function "inset" "none")
    (box-sizing class--sizing-box)
    (break-after class--break-mode)
    (break-before class--break-mode)
    (break-inside class--break-inside-mode)
    (caption-side
     "block-end" "block-start" "bottom" "inline-end" "inline-start"
     "top")
    (caret-color class--color "auto")
    (clear float "both")
    (clip-path class--geometry-box class--shape "none" "url(")
    (color class--color)
    (color-scheme "dark" "light" "normal")
    (column-count class--math-function "auto")
    (column-fill "auto" "balance" "balance-all")
    (column-gap class--math-function "normal")
    (column-rule
     column-rule-color column-rule-style column-rule-width)
    (column-rule-color class--color)
    (column-rule-style border-style)
    (column-rule-width class--line-width)
    (column-span "all" "none")
    (column-width class--math-function "auto")
    (columns column-count column-width)
    (contain
     "content" "layout" "none" "paint" "size" "strict" "style")
    (contain-intrinsic-block-size class--math-function "auto" "none")
    (contain-intrinsic-height class--math-function "auto" "none")
    (contain-intrinsic-inline-size class--math-function "auto" "none")
    (contain-intrinsic-size class--math-function "auto" "none")
    (contain-intrinsic-width class--math-function "auto" "none")
    (content
     class--image
     "close-quote" "no-close-quote" "no-open-quote" "none" "normal"
     "open-quote" "attr(" "counter(" "counters(")
    (content-visibility "auto" "hidden" "visible")
    (counter-increment class--math-function "none")
    (counter-reset class--math-function "none")
    (counter-set class--math-function "none")
    (cursor
     class--math-function
     "alias" "all-scroll" "auto" "cell" "col-resize" "context-menu"
     "copy" "crosshair" "default" "e-resize" "ew-resize" "grab"
     "grabbing" "help" "move" "n-resize" "ne-resize" "nesw-resize"
     "no-drop" "none" "not-allowed" "ns-resize" "nw-resize"
     "nwse-resize" "pointer" "progress" "row-resize" "s-resize"
     "se-resize" "sw-resize" "text" "vertical-text" "w-resize" "wait"
     "zoom-in" "zoom-out" "url(")
    (direction "ltr" "rtl")
    (display
     "block" "contents" "flex" "flow" "flow-root" "grid" "inline"
     "inline-block" "inline-flex" "inline-grid" "inline-table"
     "list-item" "none" "ruby" "ruby-base" "ruby-base-container"
     "ruby-text" "ruby-text-container" "run-in" "table"
     "table-caption" "table-cell" "table-column" "table-column-group"
     "table-footer-group" "table-header-group" "table-row"
     "table-row-group")
    (empty-cells "hide" "show")
    (filter class--filter-function "none" "url(")
    (flex flex-basis flex-grow flex-shrink)
    (flex-basis
     class--math-function
     "auto" "content" "fit-content" "max-content" "min-content")
    (flex-direction "column" "column-reverse" "row" "row-reverse")
    (flex-flow flex-direction flex-wrap)
    (flex-grow class--math-function)
    (flex-shrink class--math-function)
    (flex-wrap "nowrap" "wrap" "wrap-reverse")
    (float "inline-end" "inline-start" "left" "none" "right")
    (font
     font-family font-size font-stretch font-style font-variant
     font-weight line-height)
    (font-family
     "cursive" "emoji" "fangsong" "fantasy" "math" "monospace"
     "sans-serif" "serif" "system-ui" "ui-monospace" "ui-rounded"
     "ui-sans-serif" "ui-serif")
    (font-feature-settings "normal")
    (font-kerning "auto" "none" "normal")
    (font-language-override "normal")
    (font-optical-sizing "auto" "none")
    (font-size
     class--math-function
     "large" "larger" "math" "medium" "small" "smaller" "x-large"
     "x-small" "xx-large" "xx-small" "xxx-large")
    (font-size-adjust
     class--math-function
     "cap-height" "ch-width" "ex-height" "ic-height" "ic-width" "none")
    (font-stretch
     class--math-function
     "condensed" "expanded" "extra-condensed" "extra-expanded"
     "normal" "semi-condensed" "semi-expanded" "ultra-condensed"
     "ultra-expanded")
    (font-style class--math-function "italic" "normal" "oblique")
    (font-synthesis "none" "small-caps" "style" "weight")
    (font-variant
     font-variant-alternates font-variant-caps font-variant-east-asian
     font-variant-ligatures font-variant-numeric)
    (font-variant-alternates
     "historical-forms" "normal"
     "annotation(" "character-variant(" "ornaments(" "styleset("
     "stylistic(" "swash(")
    (font-variant-caps
     "all-petite-caps" "all-small-caps" "normal" "petite-caps"
     "small-caps" "titling-caps" "unicase")
    (font-variant-east-asian
     "full-width" "jis04" "jis78" "jis83" "jis90" "normal"
     "proportional-width" "ruby" "simplified" "traditional")
    (font-variant-ligatures
     "common-ligatures" "contextual" "discretionary-ligatures"
     "historical-ligatures" "no-common-ligatures" "no-contextual"
     "no-discretionary-ligatures" "no-historical-ligatures" "none"
     "normal")
    (font-variant-numeric
     "diagonal-fractions" "lining-nums" "normal" "oldstyle-nums"
     "ordinal" "proportional-nums" "slashed-zero" "stacked-fractions"
     "tabular-nums")
    (font-variant-position "normal" "sub" "super")
    (font-variation-settings class--math-function "normal")
    (font-weight
     class--math-function "bold" "bolder" "lighter" "normal")
    (forced-color-adjust "auto" "none")
    (gap column-gap row-gap)
    (grid
     grid-auto-columns grid-auto-flow grid-auto-rows
     grid-template-areas grid-template-columns grid-template-rows)
    (grid-area
     grid-column-end grid-column-start grid-column-end grid-row-start)
    (grid-auto-columns class--grid-size)
    (grid-auto-flow "column" "dense" "row")
    (grid-auto-rows class--grid-size)
    (grid-column grid-column-end grid-column-start)
    (grid-column-end class--grid-area)
    (grid-column-start class--grid-area)
    (grid-row grid-row-end grid-row-start)
    (grid-row-end class--grid-area) (grid-row-start class--grid-area)
    (grid-template
     grid-template-areas grid-template-columns grid-template-rows)
    (grid-template-areas "none")
    (grid-template-columns class--grid-template)
    (grid-template-rows class--grid-template)
    (hanging-punctuation
     "allow-end" "first" "force-end" "last" "none")
    (height class--box-size)
    (hyphenate-character "auto")
    (hyphens "auto" "manual" "none")
    (image-orientation "from-image" "none")
    (image-rendering
     "auto" "crisp-edges" "high-quality" "pixelated" "smooth")
    (image-resolution "from-image" "snap")
    (initial-letter class--math-function "normal")
    (initial-letter-align "alphabetic" "auto" "hanging" "ideographic")
    (inline-size class--box-size)
    (inset class--math-function "auto")
    (inset-block inset-block-end inset-block-start)
    (inset-block-end class--math-function "auto")
    (inset-block-start class--math-function "auto")
    (inset-inline inset-inline-end inset-inline-start)
    (inset-inline-end class--math-function "auto")
    (inset-inline-start class--math-function "auto")
    (isolation "auto" "isolate")
    (justify-content
     class--flex-align-container class--flex-align-horiz)
    (justify-items
     class--flex-align-content class--flex-align-horiz "legacy")
    (justify-self
     class--flex-align-content class--flex-align-horiz "auto")
    (justify-tracks justify-content)
    (left class--math-function "auto")
    (letter-spacing class--math-function "notmal")
    (line-break "anywhere" "auto" "loose" "normal" "strict")
    (-webkit-line-clamp class--math-function "none")
    (line-height class--math-function "-moz-block-height" "normal")
    (line-height-step class--math-function)
    (list-style list-style-image list-style-position list-style-type)
    (list-style-image class--image "none")
    (list-style-position "inside" "outside")
    (list-style-type
     "arabic-indic" "-moz-arabic-indic" "armenian" "bengali"
     "-moz-bengali" "cambodian" "circle" "cjk-decimal"
     "cjk-earthly-branch" "-moz-cjk-earthly-branch"
     "cjk-heavenly-stem" "-moz-cjk-heavenly-stem" "cjk-ideographic"
     "decimal" "decimal-leading-zero" "devanagari" "-moz-devanagari"
     "disc" "disclosure-closed" "disclosure-open"
     "-moz-ethiopic-halehame" "-moz-ethiopic-halehame-am"
     "ethiopic-halehame-ti-er" "-moz-ethiopic-halehame-ti-er"
     "ethiopic-halehame-ti-et" "-moz-ethiopic-halehame-ti-et"
     "ethiopic-numeric" "georgian" "gujarati" "-moz-gujarati"
     "gurmukhi" "-moz-gurmukhi" "hangul" "-moz-hangul"
     "hangul-consonant" "-moz-hangul-consonant" "hebrew" "hiragana"
     "hiragana-iroha" "japanese-formal" "japanese-informal" "kannada"
     "-moz-kannada" "katakana" "katakana-iroha" "khmer"
     "korean-hangul-formal" "korean-hanja-formal"
     "korean-hanja-informal" "lao" "-moz-lao" "lower-alpha"
     "lower-armenian" "lower-greek" "lower-latin" "lower-roman"
     "malayalam" "-moz-malayalam" "mongolian" "myanmar" "-moz-myanmar"
     "none" "oriya" "-moz-oriya" "persian" "-moz-persian"
     "simp-chinese-formal" "simp-chinese-informal" "square" "tamil"
     "-moz-tamil" "telugu" "-moz-telugu" "thai" "-moz-thai" "tibetan"
     "trad-chinese-formal" "trad-chinese-informal" "upper-alpha"
     "upper-armenian" "upper-latin" "upper-roman" "urdu" "-moz-urdu"
     "symbols(")
    (margin margin-bottom margin-left margin-right margin-top)
    (margin-block margin-block-end margin-block-start)
    (margin-block-end class--math-function "auto")
    (margin-block-start class--math-function "auto")
    (margin-bottom class--math-function "auto")
    (margin-inline margin-inline-end margin-inline-start)
    (margin-inline-end class--math-function "auto")
    (margin-inline-start class--math-function "auto")
    (margin-left class--math-function "auto")
    (margin-right class--math-function "auto")
    (margin-top class--math-function "auto")
    (margin-trim "all" "in-flow" "none")
    (mask
     mask-clip mask-composite mask-image mask-mode mask-origin
     mask-position mask-repeat mask-size)
    (mask-border
     mask-border-mode mask-border-outset mask-border-repeat
     mask-border-slice mask-border-source mask-border-width)
    (mask-border-mode mask-type)
    (mask-border-outset class--math-function)
    (mask-border-repeat border-image-repeat)
    (mask-border-slice class--math-function "fill")
    (mask-border-source class--image "none")
    (mask-border-width class--math-function "auto")
    (mask-clip class--geometry-box "no-clip" "text")
    (mask-composite "add" "exclude" "intersect" "subtract")
    (mask-image class--image "none")
    (mask-mode mask-type "alpha")
    (mask-origin class--geometry-box)
    (mask-position class--position)
    (mask-repeat background-repeat)
    (mask-size background-size)
    (mask-type "alpha" "luminance")
    (masonry-auto-flow "definite-first" "next" "ordered" "pack")
    (math-depth class--math-function "auto-add" "add(")
    (math-shift "compact" "normal") (math-style "compact" "normal")
    (max-block-size class--box-size-limit "none")
    (max-height class--box-size-limit "none")
    (max-inline-size class--box-size-limit "none")
    (max-width class--box-size-limit "none")
    (min-block-size class--box-size-limit "auto")
    (min-height class--box-size-limit "auto")
    (min-inline-size class--box-size-limit "auto")
    (min-width class--box-size-limit "auto")
    (mix-blend-mode class--blend-mode "plus-darker" "plus-lighter")
    (object-fit "contain" "cover" "fill" "none" "scale-down")
    (object-position class--position)
    (offset
     offset-anchor offset-distance offset-path offset-position
     offset-rotate)
    (offset-anchor class--position "auto")
    (offset-distance class--math-function)
    (offset-path
     class--coord-box class--math-function class--shape
     "none" "ray(" "url(")
    (offset-position class--position "auto")
    (offset-rotate class--math-function "auto" "reverse")
    (opacity class--math-function)
    (order class--math-function)
    (orphans class--math-function)
    (outline outline-color outline-style outline-width)
    (outline-color class--color "invert")
    (outline-offset class--math-function)
    (outline-style class--line-style "auto")
    (outline-width class--line-width)
    (overflow overflow-x overflow-y)
    (overflow-anchor "auto" "none")
    (overflow-block class--overflow-mode)
    (overflow-clip-margin class--math-function class--visual-box)
    (overflow-inline class--overflow-mode)
    (overflow-wrap "anywhere" "break-word" "normal")
    (overflow-x class--overflow-mode "clip")
    (overflow-y class--overflow-mode "clip")
    (overscroll-behavior overscroll-behavior-x overscroll-behavior-y)
    (overscroll-behavior-block class--scroll-behavior)
    (overscroll-behavior-inline class--scroll-behavior)
    (overscroll-behavior-x class--scroll-behavior)
    (overscroll-behavior-y class--scroll-behavior)
    (padding padding-bottom padding-left padding-right padding-top)
    (padding-block padding-block-end padding-block-start)
    (padding-block-end class--math-function)
    (padding-block-start class--math-function)
    (padding-bottom class--math-function)
    (padding-inline padding-inline-end padding-inline-start)
    (padding-inline-end class--math-function)
    (padding-inline-start class--math-function)
    (padding-left class--math-function)
    (padding-right class--math-function)
    (padding-top class--math-function)
    (paint-order "fill" "makers" "normal" "stroke")
    (perspective class--math-function "none")
    (perspective-origin
     class--math-function
     "center" "left" "right" "x-position" "y-position")
    (place-content align-content justify-content)
    (place-items align-items justify-items)
    (place-self align-self justify-self)
    (pointer-events
     "all" "auto" "bounding-box" "fill" "none" "painted" "stroke"
     "visible" "visiblefill" "visiblepainted" "visiblestroke")
    (position "absolute" "fixed" "static" "sticky" "relative")
    (print-color-adjust "economy" "exact")
    (quotes "auto" "none")
    (resize class--axis "both" "none")
    (right class--math-function "auto")
    (rotate class--math-function "none")
    (row-gap class--math-function)
    (ruby-align "center" "space-around" "space-between" "start")
    (ruby-position "alternate" "inter-character" "over" "under")
    (scale class--math-function "none")
    (scroll-behavior "auto" "smooth")
    (scroll-margin
     scroll-margin-bottom scroll-margin-left scroll-margin-right
     scroll-margin-top)
    (scroll-margin-block
     scroll-margin-block-end scroll-margin-block-start)
    (scroll-margin-block-end class--math-function)
    (scroll-margin-block-start class--math-function)
    (scroll-margin-bottom class--math-function)
    (scroll-margin-inline
     scroll-margin-inline-end scroll-margin-inline-start)
    (scroll-margin-inline-end class--math-function)
    (scroll-margin-inline-start class--math-function)
    (scroll-margin-left class--math-function)
    (scroll-margin-right class--math-function)
    (scroll-margin-top class--math-function)
    (scroll-padding
     scroll-padding-bottom scroll-padding-left scroll-padding-right
     scroll-padding-top)
    (scroll-padding-block
     scroll-padding-block-end scroll-padding-block-start)
    (scroll-padding-block-end class--math-function "auto")
    (scroll-padding-block-start class--math-function "auto")
    (scroll-padding-bottom class--math-function "auto")
    (scroll-padding-inline
     scroll-padding-inline-end scroll-padding-inline-start)
    (scroll-padding-inline-end class--math-function "auto")
    (scroll-padding-inline-start class--math-function "auto")
    (scroll-padding-left class--math-function "auto")
    (scroll-padding-right class--math-function "auto")
    (scroll-padding-top class--math-function "auto")
    (scroll-snap-align "center" "end" "none" "start")
    (scroll-snap-stop "always" "normal")
    (scroll-snap-type
     "block" "both" "inline" "mandatory" "none" "proximity" "x" "y")
    (scroll-timeline scroll-timeline-axis scroll-timeline-name)
    (scroll-timeline-axis class--axis)
    (scroll-timeline-name "none")
    (scrollbar-color class--color "auto")
    (scrollbar-gutter "auto" "both-edges" "stable")
    (scrollbar-width "auto" "none" "thin")
    (shape-image-threshold class--math-function)
    (shape-margin class--math-function)
    (shape-outside
     class--gradient class--shape class--shape-box "none" "url(")
    (tab-size class--math-function)
    (table-layout "auto" "fixed")
    (text-align class--align-x "justify" "justify-all" "match-parent")
    (text-align-last class--align-x "auto" "justify" "match-parent")
    (text-combine-upright class--math-function "all" "digits" "none")
    (text-decoration
     text-decoration-color text-decoration-line text-decoration-style
     text-decoration-thickness)
    (text-decoration-color class--color)
    (text-decoration-line
     "line-through" "none" "overline" "underline")
    (text-decoration-skip
     "auto" "box-decoration" "edges" "leading-spaces" "none" "objects"
     "spaces" "trailing-spaces")
    (text-decoration-skip-ink "all" "auto" "none")
    (text-decoration-style class--decoration-style)
    (text-decoration-thickness
     class--math-function "auto" "from-font")
    (text-emphasis text-emphasis-color text-emphasis-style)
    (text-emphasis-color class--color)
    (text-emphasis-position "left" "over" "right" "under")
    (text-emphasis-style
     "circle" "dot" "double-circle" "filled" "none" "open" "sesame"
     "triangle")
    (-webkit-text-fill-color class--color)
    (text-indent class--math-function "each-line" "hanging")
    (text-justify "auto" "inter-character" "inter-word" "none")
    (text-orientation "mixed" "sideways" "upright")
    (text-overflow "clip" "ellipsis" "fade" "fade(")
    (text-rendering
     "auto" "geometricprecision" "optimizelegibility" "optimizespeed")
    (text-shadow class--color class--math-function)
    (text-size-adjust class--math-function "auto" "none")
    (text-transform
     "capitalize" "full-size-kana" "full-width" "lowercase" "none"
     "uppercase")
    (text-underline-offset class--math-function "auto")
    (text-underline-position
     "above" "auto" "auto-pos" "below" "from-font" "left" "right"
     "under")
    (top class--math-function "auto")
    (touch-action
     "auto" "manipulation" "none" "pan-down" "pan-left" "pan-right"
     "pan-up" "pan-x" "pan-y" "pinch-zoom")
    (transform class--transform-function "none")
    (transform-box class--transform-box)
    (transform-origin class--position)
    (transform-style "flat" "preserve-3d")
    (transition
     transition-delay transition-duration transition-property
     transition-timing-function)
    (transition-delay class--math-function)
    (transition-duration class--math-function)
    (transition-property web-capf--css-props "all" "none")
    (transition-timing-function class--easing-function)
    (translate class--math-function "none")
    (unicode-bidi
     "bidi-override" "embed" "isolate" "isolate-override" "normal"
     "plaintext")
    (user-select "all" "auto" "contain" "none" "text")
    (vertical-align
     class--math-function
     "baseline" "bottom" "middle" "sub" "super" "text-bottom"
     "text-top" "top")
    (visibility "collapse" "hidden" "visible")
    (white-space
     "break-spaces" "normal" "nowrap" "pre" "pre-line" "pre-wrap")
    (widows class--math-function)
    (width class--box-size)
    (will-change
     web-capf--css-props "auto" "contents" "scroll-position")
    (word-break "break-all" "keep-all" "normal")
    (word-spacing class--math-function "normal")
    (writing-mode
     "horizontal-tb" "sideways-lr" "sideways-rl" "vertical-lr"
     "vertical-rl")
    (z-index class--math-function "auto"))
  "Alist of css3 property names and values or function names.")

(defconst web-capf-css-global-prop-vals
  '("inherit" "initial" "revert" "revert-layer" "unset")
  "List of css3 global property values.")

(defconst web-capf-css-global-prop-flags
  '("important")
  "List of css3 global property flags.")

(defconst web-capf-css-global-prop-funcs
  '("env(" "var(")
  "List of css3 global property value function names.")

(defconst web-capf-css-prop-func-args
  '((abs class--math-function)
    (acos class--math-function)
    (add class--math-function)
    (asin class--math-function)
    (atan class--math-function)
    (atan2 class--math-function)
    (attr web-capf--html-attrs)
    (blur class--math-function)
    (brightness class--math-function)
    (calc class--math-function)
    (circle
     class--math-function class--positon
     "at" "closest-side" "farthest-side")
    (clamp class--math-function)
    (color
     class--math-function
     "a98-rgb" "display-p3" "none" "prophoto-rgb" "rec2020" "srgb"
     "srgb-linear" "xyz" "xyz-d50" "xyz-d65")
    (conic-gradient class--conic-gradient)
    (contrast class--math-function)
    (cos class--math-function)
    (counter list-style-type)
    (counters list-style-type)
    (cross-fade class--color class--image class--math-function)
    (cubic-bezier class--math-function)
    (drop-shadow class--color class--math-function)
    (ellipse
     class--math-function class--positon
     "at" "closest-side" "farthest-side")
    (env
     class--math-function
     "safe-area-inset-bottom" "safe-area-inset-left"
     "safe-area-inset-right" "safe-area-inset-top"
     "titlebar-area-height" "titlebar-area-width" "titlebar-area-x"
     "titlebar-area-y")
    (exp class--math-function)
    (fade class--math-function)
    (fit-content class--math-function)
    (grayscale class--math-function)
    (hsl class--math-function "none")
    (hsla class--math-function "none")
    (hue-rotate class--math-function)
    (hwb class--math-function "none")
    (hypot class--math-function)
    (image class--color "ltr" "rtl" "url(")
    (image-set class--image class--math-function)
    (inset class--math-function "round")
    (invert class--math-function)
    (lab class--math-function "none")
    (lch class--math-function "none")
    (linear class--math-function)
    (linear-gradient class--linear-gradient)
    (log class--math-function)
    (matrix class--math-function)
    (matrix3d class--math-function)
    (max class--math-function)
    (min class--math-function)
    (minmax class--math-function "auto" "max-content" "min-content")
    (oklab class--math-function "none")
    (oklch class--math-function "none")
    (opacity class--math-function)
    (path "evenodd" "nonzero")
    (perspective class--math-function "none")
    (polygon class--math-function "evenodd" "nonzero")
    (pow class--math-function)
    (radial-gradient class--radial-gradient)
    (ray
     class--math-function
     "closest-corner" "closest-side" "contain" "farthest-corner"
     "farthest-side")
    (repeat
     class--math-function
     "auto" "auto-fill" "auto-fit" "max-content" "min-content"
     "fit-content(" "minmax(")
    (repeating-conic-gradient class--conic-gradient)
    (repeating-linear-gradient class--linear-gradient)
    (repeating-radial-gradient class--radial-gradient)
    (rgb class--math-function "none")
    (rgba class--math-function "none")
    (rotate class--math-function)
    (rotate3d class--math-function)
    (rotateX class--math-function)
    (rotateY class--math-function)
    (rotateZ class--math-function)
    (saturate class--math-function)
    (scale class--math-function)
    (scale3d class--math-function)
    (scaleX class--math-function)
    (scaleY class--math-function)
    (scaleZ class--math-function)
    (sepia class--math-function)
    (sign class--math-function)
    (sin class--math-function)
    (skew class--math-function)
    (skewX class--math-function)
    (skewY class--math-function)
    (sqrt class--math-function)
    (steps
     class--math-function
     "end" "jump-both" "jump-end" "jump-none" "jump-start" "start")
    (symbols
     class--image "alphabetic" "cyclic" "fixed" "numeric" "symbolic")
    (tan class--math-function)
    (translate class--math-function)
    (translate3d class--math-function)
    (translateX class--math-function)
    (translateY class--math-function)
    (translateZ class--math-function))
  "Alist of css3 property value function names and arguments.")

(defconst web-capf-css-val-classes
  '((align-x "center" "end" "left" "right" "start")
    (axis "block" "horizontal" "inline" "vertical")
    (blend-mode
     "color" "color-burn" "color-dodge" "darken" "difference"
     "exclusion" "hard-light" "hue" "lighten" "luminosity" "multiply"
     "normal" "overlay" "saturation" "screen" "soft-light")
    (box-size
     class--math-function
     "auto" "fit-content" "max-content" "min-content" "fit-content(")
    (box-size-limit
     class--math-function
     "max-content" "fit-content" "min-content" "stretch"
     "fit-content(")
    (break-inside-mode
     "auto" "avoid" "avoid-column" "avoid-page" "avoid-region")
    (break-mode
     class--break-inside-mode
     "all" "column" "left" "page" "region" "right")
    (conic-gradient
     class--color class--gradient-position
     "decreasing" "from" "hsl" "hue" "hwb" "in" "increasing" "lab"
     "lch" "longer" "oklab" "oklch" "shorter" "specified" "srgb"
     "srgb-linear" "xyz" "xyz-d50" "xyz-d65")
    (color
     ;; special keywords
     "currentcolor" "transparent"
     ;; keywords
     "aliceblue" "antiquewhite" "aqua" "aqua" "aquamarine" "azure"
     "beige" "bisque" "black" "blanchedalmond" "blue" "blueviolet"
     "brown" "burlywood" "cadetblue" "chartreuse" "chocolate" "coral"
     "cornflowerblue" "cornsilk" "crimso" "cyan" "darkblue" "darkcyan"
     "darkgoldenrod" "darkgray" "darkgreen" "darkgrey" "darkkhaki"
     "darkmagenta" "darkolivegreen" "darkorange" "darkorchid"
     "darkred" "darksalmon" "darkseagreen" "darkslateblue"
     "darkslategray" "darkslategrey" "darkturquoise" "darkviolet"
     "deeppink" "deepskyblue" "dimgray" "dimgrey" "dodgerblue"
     "firebrick" "floralwhite" "forestgreen" "fuchsia" "fuchsia"
     "gainsboro" "ghostwhite" "gold" "goldenrod" "gray" "green"
     "greenyellow" "grey" "honeydew" "hotpink" "indianred" "indigo"
     "ivory" "khaki" "lavender" "lavenderblush" "lawngreen"
     "lemonchiffon" "lightblue" "lightcoral" "lightcyan"
     "lightgoldenrodyellow" "lightgray" "lightgreen" "lightgrey"
     "lightpink" "lightsalmon" "lightseagreen" "lightskyblue"
     "lightslategray" "lightslategrey" "lightsteelblue" "lightyellow"
     "lime" "limegreen" "linen" "magenta" "maroon" "mediumaquamarine"
     "mediumblue" "mediumorchid" "mediumpurple" "mediumseagreen"
     "mediumslateblue" "mediumspringgreen" "mediumturquoise"
     "mediumvioletred" "midnightblue" "mintcream" "mistyrose"
     "moccasin" "navajowhite" "navy" "oldlace" "olive" "olivedrab"
     "orange" "orangered" "orchid" "palegoldenrod" "palegreen"
     "paleturquoise" "palevioletred" "papayawhip" "peachpuff" "peru"
     "pink" "plum" "powderblue" "purple" "rebeccapurple" "red"
     "rosybrown" "royalblue" "saddlebrown" "salmon" "sandybrown"
     "seagreen" "seashell" "sienna" "silver" "skyblue" "slateblue"
     "slategray" "slategrey" "snow" "springgreen" "steelblue" "tan"
     "teal" "thistle" "tomato" "turquoise" "violet" "wheat" "white"
     "whitesmoke" "yellow" "yellowgreen"
     ;; system colors
     "activetext" "buttonborder" "buttonface" "buttontext" "canvas"
     "canvastext" "field" "fieldtext" "graytext" "highlight"
     "highlighttext" "linktext" "mark" "marktext" "visitedtext"
     ;; functions
     "color(" "hsl(" "hsla(" "hwb(" "lab(" "lch(" "oklab(" "oklch("
     "rgb(" "rgba(")
    (coord-box class--visual-box class--draw-box)
    (decoration-style class--line-style-base "wavy")
    (draw-box "fill-box" "stroke-box" "view-box")
    (easing-function
     "ease" "ease-in" "ease-in-out" "ease-out" "linear" "step-end"
     "step-start" "cubic-bezier(" "linear(" "steps(")
    (filter-function
     "blur(" "brightness(" "contrast(" "drop-shadow(" "grayscale("
     "hue-rotate(" "invert(" "opacity(" "saturate(" "sepia(")
    (flex-align-base
     "center" "end" "flex-end" "flex-start" "normal" "safe" "start"
     "stretch" "unsafe")
    (flex-align-baseline "baseline" "first" "last")
    (flex-align-horiz "left" "right")
    (flex-align-container
     class--flex-align-base
     "space-around" "space-between" "space-evenly")
    (flex-align-content
     class--flex-align-base class--flex-align-baseline
     "self-end" "self-start")
    (geometry-box class--coord-box "margin-box")
    (gradient
     "conic-gradient(" "linear-gradient(" "radial-gradient("
     "repeating-conic-gradient(" "repeating-linear-gradient("
     "repeating-radial-gradient(")
    (gradient-position
     class--position
     "at" "end" "start" "x-end" "x-start" "y-end" "y-start")
    (grid-area class--math-function "auto" "span")
    (grid-size
     class--math-function
     "auto" "max-content" "min-content" "fit-content(" "minmax(")
    (grid-template
     class--grid-size "masonry" "none" "subgrid" "repeat(")
    (image
     class--gradient
     "cross-fade(" "element(" "image(" "image-set(" "url(")
    (keyframe-elems class--math-function "from" "to")
    (line-style-base "dashed" "dotted" "double" "solid")
    (line-style
     class--line-style-base "groove" "inset" "none" "outset" "ridge")
    (line-width class--math-function "medium" "thick" "thin")
    (linear-gradient
     class--color class--math-function
     "bottom" "left" "right" "to" "top")
    (math-function
     "abs(" "acos(" "asin(" "atan(" "atan2(" "calc(" "clamp(" "cos("
     "exp(" "hypot(" "log(" "max(" "min(" "pow(" "sign(" "sin("
     "sqrt(" "tan(")
    (nth class--math-function "even" "odd")
    (overflow-mode "auto" "hidden" "scroll" "visible")
    (position class--position-x class--position-y)
    (position-x class--math-function "center" "left" "right")
    (position-y class--math-function "bottom" "center" "top")
    (radial-gradient
     class--color class--gradient-position
     "closest-corner" "closest-side" "farthest-corner" "farthest-side")
    (scroll-behavior "auto" "contain" "none")
    (shape "circle(" "ellipse(" "inset(" "path(" "polygon(")
    (shape-box class--visual-box "margin-box")
    (sizing-box "border-box" "content-box")
    (transform-box class--sizing-box class--draw-box)
    (transform-function
     "matrix(" "matrix3d(" "perspective(" "rotate(" "rotate3d("
     "rotateX(" "rotateY(" "rotateZ(" "scale(" "scale3d(" "scaleX("
     "scaleY(" "scaleZ(" "skew(" "skewX(" "skewY(" "translate("
     "translate3d(" "translateX(" "translateY(" "translateZ(")
    (visual-box class--sizing-box "padding-box"))
  "Alist of css3 value classes.")

(defconst web-capf-html-syntax-regexp
  "\\(<!--\\|[ \t\n]+\\|[<>=\"']\\)"
  "Regexp to parse html.")

(defconst web-capf-html-decls-regexp
  "<!\\[?[A-Za-z]*"
  "Regexp that matches to html declarations, or cdata sections.")

(defconst web-capf-html-insts-regexp
  "<\\?[A-Za-z]*"
  "Regexp that matches to html processing instructions for xml.")

(defconst web-capf-html-tags-regexp
  "<[-0-9A-Za-z]*"
  "Regexp that matches to html tag name parts.")

(defconst web-capf-html-attr-decls-regexp
  "<!\\([A-Za-z]+\\)[ \t\n]+"
  "Regexp that matches to html declaration parts followed by attributes.")

(defconst web-capf-html-attr-tags-regexp
  "<\\([-0-9A-Za-z]+\\)\\([ \t\n]\\|<!--.*?-->\\)+"
  "Regexp that matches to html tag name parts followed by attributes.")

(defconst web-capf-html-attrs-regexp
  "\\([ \t\n]\\|<!--.*?-->\\)\\([-0-9A-Za-z]+\\)\\([ \t\n]\\|<!--.*?-->\\)*="
  "Regexp that matches to html attribute parts.")

(defconst web-capf-css-syntax-regexp
  "\\([][(){}#.=:;\"']\\|/\\*\\|@media\\|@keyframes\\)"
  "Regexp to parse css.")

(defconst web-capf-css-at-keywords-regexp
  "@[-0-9A-Za-z]*"
  "Regexp that matches to css @-keyword parts.")

(defconst web-capf-css-pseudo-elem-sels-regexp
  "::[-0-9A-Za-z]*"
  "Regexp that matches to css pseudo element selectors parts.")

(defconst web-capf-css-pseudo-class-sels-regexp
  ":[-0-9A-Za-z]*"
  "Regexp that matches to css pseudo class selector parts.")

(defconst web-capf-css-id-or-class-sels-regexp
  "[#.][-0-9A-Za-z]*"
  "Regexp that matches to css id or class selector parts.")

(defconst web-capf-css-props-regexp
  "[^-0-9A-Za-z:#.]\\([-0-9A-Za-z]+\\)\\([ \t\n]\\|/\\*.*?\\*/\\)*:"
  "Regexp that matches to css property name parts.")

(defconst web-capf-css-sel-tags-regexp
  "[^-0-9A-Za-z:#.]\\([-0-9A-Za-z]+\\)\\(/\\*.*?\\*/\\)*[[#.:]"
  "Regexp that matches to css tag name parts followed by selectors.")

(defconst web-capf-css-attr-sels-regexp
  "\\[\\(/\\*.*?\\*/\\)*\\([-0-9A-Za-z]+\\)\\(/\\*.*?\\*/\\)*[~|^\\$*]?="
  "Regexp that matches to css attribute selector parts.")

(defconst web-capf-css-sel-funcs-regexp
  ":\\(/\\*.*?\\*/\\)*\\([-0-9A-Za-z]+\\)\\(/\\*.*?\\*/\\)*("
  "Regexp that matches to css pseudo secector function parts.")

(defconst web-capf-css-prop-funcs-regexp
  "[^-0-9A-Za-z]\\([-0-9A-Za-z]+\\)\\([ \t\n]\\|/\\*.*?\\*/\\)*("
  "Regexp that matches to css property value function parts.")

(defconst web-capf-css-prop-flags-regexp
  "![-0-9A-Za-z]*"
  "Regexp that matches to css propery flag parts.")

(defun web-capf--get-type-for-web-mode (point)
  "Return completion type for `web-mode' at POINT."
  ;; check text property to determine subparts in html (css, javascript)
  (or (get-text-property point 'part-side)
      ;; or check text property to find subblocks in html (php, erb, ejs etc)
      (when (get-text-property point 'block-side)
        ;; refer to web-mode-engine for subblocks
        (let ((engine (intern web-mode-engine)))
          (or (alist-get engine web-capf-engines-and-modes)
              engine)))
      ;; or check web-mode-content-type (whole file of html, css)
      (unless (eq web-mode-content-type "")
        (intern web-mode-content-type))))

(defun web-capf--push (elem list)
  "Same as `push', except modifying LIST destructively.
According to that side effect, LIST must be terminated by nil as a sentinel.
Note that it is safer to initialize by \"(list nil)\" or \"(cons nil nil)\";
generating new empty \"(nil)\" list to avoid self-modification.
Return whole list after pushed."
  (setcdr list (cons (car list) (cdr list)))
  (setcar list elem)
  list)

(defun web-capf--pop (list)
  "Same as `pop', except modifying LIST destructively.
If LIST has only nil as a member, it is treated as an empty list.
Return removed elem, or nil if empty."
  (let ((elem (car list)))
    (setcar list (cadr list))
    (setcdr list (cddr list))
    elem))

(defun web-capf--syntaxp (syntax types)
  "Return non-nil if the most recent elem in SYNTAX is that of TYPES,
specified as a list of type symbols or a single type symbol."
  (cond
   ((and types (listp types))
    (catch 'found
      (mapc
       (lambda (type)
         (when (eq (caar syntax) type)
           (throw 'found t)))
       types)
      nil))
   (t (eq (caar syntax) types))))

(defun web-capf--clean-syntax (syntax types)
  "From SYNTAX, remove all recent elems of TYPES,
specified as a list of type symbols or a single type symbol.
Return list of removed elems."
  (let ((elems ()))
    (while (web-capf--syntaxp syntax types)
      (push (web-capf--pop syntax) elems))
    (nreverse elems)))

(defun web-capf--looking-back (regexp &optional limit start)
  "Same as `looking-back', except avoiding greedy stretch on lazy match,
and returning whole match string if match.
Also try to look back from START, if specified."
  (if start
      (save-excursion
        (goto-char start)
        (web-capf--looking-back regexp limit))
    (when (looking-back regexp limit)
      ;; looking-back extends the match to fit current point even if
      ;; regexp has lazy match specifiers
      (when-let*
          ((match (match-string-no-properties 0))
           (exact (and (string-match regexp match) (match-string 0 match))))
        ;; so compare the whole match string with that not extended
        (and (string= exact match) match)))))

(defun web-capf--get-html-tags-from-rule (hierarchy rule)
  "Get html tags list for RULE under HIERARCHY."
  (let ((self (car hierarchy))
        (parent (cadr hierarchy))
        (ancestor (cdr hierarchy)))
    (cond
     ((eq rule 'web-capf--section)
      web-capf-html-section-tags)
     ((eq rule 'web-capf--block)
      web-capf-html-block-tags)
     ((eq rule 'web-capf--inline)
      web-capf-html-inline-tags)
     ((symbolp rule)
      (list rule))
     ((eq (car rule) 'web-capf--norecurse)
      (remq self (web-capf--get-html-tags-from-rules hierarchy (cdr rule))))
     ((eq (car rule) 'web-capf--ancestor)
      (mapcar
       (lambda (tag)
         (when (memq (cadr rule) hierarchy) tag))
       (web-capf--get-html-tags-from-rules hierarchy (cddr rule))))
     ((eq (car rule) 'web-capf--parent-rule)
      (when-let
          ((available (web-capf--get-html-tags ancestor)))
        (mapcar
         (lambda (tag)
           (when (memq tag available) tag))
         (web-capf--get-html-tags-from-rules ancestor (cdr rule))))))))

(defun web-capf--get-html-tags-from-rules (hierarchy rules)
  "Get html tags list for RULES under HIERARCHY."
  (apply 'append
         (mapcar
          (lambda (rule)
            (web-capf--get-html-tags-from-rule hierarchy rule))
          rules)))

(defun web-capf--get-html-tags (hierarchy)
  "Get html tags list under HIERARCHY."
  (seq-uniq
   (append
    (when-let
        ((rule (cdr (assq (car hierarchy) web-capf-html-autoclose-tags))))
      (when (or (not (cdr rule))
                (when-let
                    ((parent (cadr hierarchy)))
                  (memq parent (cdr rule))))
        (car rule)))
    (web-capf--get-html-tags-from-rules
     hierarchy
     (cdr (assq (car hierarchy) web-capf-html-tag-hierarchies))))))

(defun web-capf--get-http-attr-vals (tag attr)
  "Get html keyword list for TAG and ATTR from `web-capf-html-attr-vals'."
  (when-let*
      ((vals (alist-get attr web-capf-html-attr-vals))
       (val (car vals)))
    (cond
     ;; symbol: alias to anoter attribute
     ((symbolp val)
      (alist-get val web-capf-html-attr-vals))
     ;; alist
     ((listp val)
      (if tag
          ;; get from attribute and tag name
          (or (alist-get tag vals)
              ;; use t in case not found tag name
              (alist-get t vals))
        ;; get all entries if tag name empty
        (seq-uniq (apply 'append (mapcar 'cdr vals)))))
     ;; maybe string: get only from attribute name
     (t vals))))

(defun web-capf--get-css-vals (prop alist)
  "Get css keyword list for PROP from ALIST.
ALIST can be `web-capf-css-props-and-vals' for property values,
`web-capf-css-prop-func-args' for property value function arguments,
or `web-capf-css-sel-func-args' for selector function arguments.
NOTE: From inside this function, may called recursively with
`web-capf-css-val-classes' to expand value classes,
but not expected to be specified externally."
  (seq-uniq
   (apply
    'append
    (mapcar
     (lambda (val)
       (cond
        ;; string
        ((stringp val) (list val))
        ;; class: get from class definition
        ((when-let*
             ((name (symbol-name val))
              (klass (and (string-match "^class--\\(.*\\)$" name)
                          (intern (match-string 1 name))))
              (compl (web-capf--get-css-vals klass web-capf-css-val-classes)))
           compl))
        ;; html attributes
        ((eq val 'web-capf--html-attrs)
         (apply 'append (cons web-capf-html-global-attrs
                              (mapcar 'cdr web-capf-html-tags-and-attrs))))
        ;; css selectors: no completions here
        ((eq val 'web-capf--sels)
         nil)
        ;; css properties
        ((eq val 'web-capf--css-props)
         (mapcar (lambda (elem) (symbol-name (car elem)))
                 web-capf-css-props-and-vals))
        ;; others: alias to another property
        (t
         (web-capf--get-css-vals val web-capf-css-props-and-vals))))
     (alist-get prop alist)))))

(defun web-capf--open-syntax-html (syntax elem)
  "Open ELEM on SYNTAX stack, under the html syntax rules."
  (cond
   ;; NOTE: To check nesting, elems with open and close, always must be pushed
   ;;       (and popped), although their kind might be changed by contexts.
   ;;       The other elems for single status might be thrown away.
   ((eq (car elem) 'ang-bracket)
    (cond
     ((web-capf--syntaxp syntax nil)
      ;; outside tags: tag parts
      (web-capf--push (cons 'tag (cdr elem)) syntax))
     (t
      ;; others: ignore
      (web-capf--push elem syntax))))
   ((eq (car elem) 'space)
    (cond
     ((web-capf--syntaxp syntax 'tag)
      ;; tag parts: turn into attribute parts
      (web-capf--push (cons 'attr (cdr elem)) syntax))
     ((web-capf--syntaxp syntax 'avalue)
      ;; attribute value parts: end attribute values
      (web-capf--clean-syntax syntax 'avalue))))
   ((eq (car elem) 'equal)
    (when (web-capf--syntaxp syntax 'attr)
      ;; attribute parts: turn into attribute value parts
      (web-capf--push (cons 'avalue (cdr elem)) syntax)))
   ((memq (car elem) '(string comment))
    (web-capf--push elem syntax))))

(defun web-capf--close-syntax-html (syntax key)
  "Close the most recent elem of type KEY on SYNTAX stack,
under the html syntax rules."
  (cond
   ((eq key 'ang-bracket)
    ;; forget attribute and its value stats
    (web-capf--clean-syntax syntax '(attr avalue))
    (when (web-capf--syntaxp syntax 'tag)
      (web-capf--pop syntax)))))

(defun web-capf--open-hierarchy-html (hierarchy tag)
  "Open TAG on HIERARCHY stack, under the html tag hierarchy rules."
  (cond
   ;; tag without close
   ((memq tag web-capf-html-noclose-tags)
    nil)
   ;; tag to be closed automatically
   ((when-let
        ((rule (cdr (assq (car hierarchy) web-capf-html-autoclose-tags))))
      (when (and (memq tag (car rule))
                 (or (not (cdr rule))
                     (when-let
                         ((parent (cadr hierarchy)))
                       (memq parent (cdr rule)))))
        (web-capf--pop hierarchy)
        (web-capf--push tag hierarchy))))
   (t
    (web-capf--push tag hierarchy))))

(defun web-capf--close-hierarchy-html (hierarchy tag)
  "Close the most rescent TAG on HIERARCHY stack."
  (when (memq tag hierarchy)
    (while (eq tag (car hierarchy))
      (web-capf--pop hierarchy))))

(defun web-capf--parse-html (&optional end)
  "Parse html before END, or current point by default."
  (save-excursion
    (let ((syntax (list nil))
          (hierarchy (list nil))
          (bound (or end (point)))
          tag-beg tag tag-close-p)
      ;; Pass 1: Traverse html from bob to end to analyze syntax.
      (goto-char (point-min))
      (catch 'parse
        ;; check keywords successively
        (while (search-forward-regexp web-capf-html-syntax-regexp bound t)
          (let ((piece (match-string-no-properties 0))
                (piece-beg (match-beginning 0))
                (piece-end (match-end 0)))
            (cond
             ;; angle brackets
             ((string= piece "<")
              (web-capf--open-syntax-html syntax (cons 'ang-bracket piece-end))
              (when-let*
                  ((pt piece-end)
                   (c (progn
                        (cond
                         ((eq (char-after pt) ?/)
                          (setq tag-close-p t)
                          (setq pt (1+ pt)))
                         (t
                          (setq tag-close-p nil)))
                        (char-after pt)))
                   (ch (downcase c)))
                (cond
                 ((and (>= ch ?a) (<= ch ?z))
                  (setq tag-beg pt)
                  (setq tag nil))
                 (t
                  (setq tag-beg nil)
                  (setq tag nil)))))
             ((string= piece ">")
              (web-capf--close-syntax-html syntax 'ang-bracket)
              (when (and tag-beg (not tag))
                (setq tag (intern
                           (buffer-substring-no-properties tag-beg piece-beg)))
                (setq tag-beg nil))
              (when tag
                (if tag-close-p
                    (web-capf--close-hierarchy-html hierarchy tag)
                  (web-capf--open-hierarchy-html hierarchy tag))))
             ;; spaces
             ((string-match "^[ \t\n]+$" piece)
              (web-capf--open-syntax-html syntax (cons 'space piece-end))
              (when (and tag-beg (not tag))
                (setq tag (intern
                           (buffer-substring-no-properties tag-beg piece-beg)))
                (setq tag-beg nil)))
             ;; equals
             ((string= piece "=")
              (web-capf--open-syntax-html syntax (cons 'equal piece-end)))
             ;; strings
             ((string= piece "\"")
              (unless (catch 'string
                        ;; search eos
                        (while (search-forward-regexp
                                "\\(\"\\|\\\\\"\\)" bound t)
                          (when (string= (match-string-no-properties 0) "\"")
                            ;; skip whole string if eos found
                            (throw 'string t))))
                ;; check only non-closed string
                (web-capf--open-syntax-html syntax (cons 'string piece-end))
                ;; exit loop cause it always reaches end
                (throw 'parse t)))
             ((string= piece "'")
              (unless (catch 'string
                        ;; search eos
                        (while (search-forward-regexp
                                "\\('\\|\\\\'\\)" bound t)
                          (when (string= (match-string-no-properties 0) "'")
                            ;; skip whole string if eos found
                            (throw 'string t))))
                ;; check only non-closed string
                (web-capf--open-syntax-html syntax (cons 'string piece-end))
                ;; exit loop cause it always reaches end
                (throw 'parse t)))
             ;; comments
             ((string= piece "<!--")
              (when (and tag-beg (not tag))
                (setq tag (intern
                           (buffer-substring-no-properties tag-beg piece-beg)))
                (setq tag-beg nil))
              ;; search eoc; skip whole comment if found
              (unless (search-forward "-->" bound t)
                ;; check only non-closed comment
                (web-capf--open-syntax-html syntax (cons 'comment piece-end))
                ;; exit loop cause it always reaches end
                (throw 'parse t)))))))
      ;; Pass 2: Decide the part type just before bound.
      (cond
       ((web-capf--syntaxp syntax 'tag)
        ;; tag parts
        (cons 'tags (apply 'append (mapcar
                                    (lambda (elem)
                                      (if (not elem) nil
                                        (list elem)))
                                    hierarchy))))
       ((web-capf--syntaxp syntax 'attr)
        ;; attribute parts
        (let ((attr-pos (cdr (web-capf--pop syntax))))
          (cons 'attr-names attr-pos)))
       ((web-capf--syntaxp syntax 'avalue)
        ;; just after attribute name
        (cons 'attr-val-start nil))
       ((web-capf--syntaxp syntax 'string)
        (web-capf--pop syntax)
        (when (web-capf--syntaxp syntax 'avalue)
          ;; inside strings in attribute values
          (when-let ((avalue-pos (cdr (web-capf--pop syntax)))
                     (tag-pos (cdar (web-capf--clean-syntax syntax 'attr))))
            (cons 'attr-vals (cons avalue-pos tag-pos)))))))))

(defun web-capf--get-html-completions ()
  "Return html completion type and keywords at point."
  (let ((syntax (web-capf--parse-html)))
    (cond
     ((not (car syntax))
      ;; outside tags: no completions
      nil)
     ((eq (car syntax) 'tags)
      (cond
       ((web-capf--looking-back web-capf-html-decls-regexp)
        ;; <! declarations
        (cons 'declaration
              (mapcar (lambda (elem) (upcase (symbol-name (car elem))))
                      web-capf-html-decls-and-attrs)))
       ((web-capf--looking-back web-capf-html-insts-regexp)
        ;; <? instructions
        (cons 'instruction web-capf-html-insts))
       ((web-capf--looking-back web-capf-html-tags-regexp)
        ;; tags
        (cons 'tag
              (mapcar 'symbol-name
                      (if (cdr syntax)
                          (web-capf--get-html-tags (cdr syntax))
                        (mapcar 'car web-capf-html-tags-and-attrs)))))))
     ((eq (car syntax) 'attr-names)
      (or
       (when-let*
           ((match (web-capf--looking-back
                    web-capf-html-attr-decls-regexp nil (cdr syntax)))
            (decl (intern (downcase (match-string 1 match)))))
         ;; attribute names for decls
         (cons 'attribute-name (alist-get decl web-capf-html-decls-and-attrs)))
       (when-let*
           ((match (web-capf--looking-back
                    web-capf-html-attr-tags-regexp nil (cdr syntax)))
            (tag (intern (match-string 1 match))))
         ;; attribute names for tags
         (cons 'attribute-name
               (append (alist-get tag web-capf-html-tags-and-attrs)
                       web-capf-html-global-attrs)))))
     ((eq (car syntax) 'attr-val-start)
      ;; just after attribute name: complete only '"'
      (list 'attribute "\""))
     ((eq (car syntax) 'attr-vals)
      (when-let*
          ((match1 (web-capf--looking-back
                    web-capf-html-attr-tags-regexp nil (cddr syntax)))
           (tag (intern (match-string 1 match1)))
           (match2 (web-capf--looking-back
                    web-capf-html-attrs-regexp nil (cadr syntax)))
           (attr (intern (match-string 2 match2))))
        ;; attribute values
        (cons 'attribute-value (web-capf--get-http-attr-vals tag attr)))))))

(defun web-capf--open-syntax-css (syntax elem)
  "Open ELEM on SYNTAX stack, under the css syntax rules."
  (cond
   ;; NOTE: To check nesting, elems with open and close, always must be pushed
   ;;       (and popped), although their kind might be changed by contexts.
   ;;       The other elems for single status might be thrown away.
   ((eq (car elem) 'bracket)
    (cond
     ((web-capf--syntaxp syntax '(sparen mquery select nil))
      ;; outside property parts: attribute selector parts
      (web-capf--push (cons 'sbracket (cdr elem)) syntax))
     (t
      ;; inside property parts: ignore
      (web-capf--push (cons 'pbracket (cdr elem)) syntax))))
   ((eq (car elem) 'paren)
    (cond
     ((web-capf--syntaxp syntax '(sparen mquery select nil))
      ;; outside property parts: pseudo selector function arguments
      (web-capf--push (cons 'sparen (cdr elem)) syntax))
     ((web-capf--syntaxp syntax '(pparen pvalue))
      ;; property value parts: property value function arguments (recursive)
      (web-capf--push (cons 'pparen (cdr elem)) syntax))
     (t
      ;; others include property parts (not value): ignore
      (web-capf--push elem syntax))))
   ((eq (car elem) 'brace)
    ;; forget selector stats to turn into property parts
    (web-capf--clean-syntax syntax 'select)
    (cond
     ((web-capf--syntaxp syntax 'media)
      ;; found @media sequence: inside media query, same as outside parts
      (setcar (car syntax) 'mquery))
     ((web-capf--syntaxp syntax 'keyframe)
      ;; found @keyframes sequence: keyframe element parts
      (setcar (car syntax) 'kfelem))
     ((web-capf--syntaxp syntax '(mquery kfelem nil))
      ;; outside property parts or keyframe element parts : property parts
      (web-capf--push (cons 'pbrace (cdr elem)) syntax))
     (t
      ;; others: ignore
      (web-capf--push elem syntax))))
   ((eq (car elem) 'shpdot)
    (when (web-capf--syntaxp syntax '(sparen mquery nil))
      ;; outside property parts: id or class selectors
      ;; NOTE: Push them to record the selector point found the earliest,
      ;;       so do not update even if found again.
      (web-capf--push (cons 'select (cdr elem)) syntax)))
   ((eq (car elem) 'equal)
    (when (web-capf--syntaxp syntax 'sbracket)
      ;; inside attribute selectors: attribute selector values
      (web-capf--push (cons 'avalue (cdr elem)) syntax)))
   ((eq (car elem) 'colon)
    (cond
     ((web-capf--syntaxp syntax '(sparen mquery nil))
      ;; outside property parts: pseudo class or element selectors
      ;; NOTE: Push them to record the selector point found the earliest,
      ;;       so do not update even if found again.
      (web-capf--push (cons 'select (cdr elem)) syntax))
     ((web-capf--syntaxp syntax 'pbrace)
      ;; inside property parts: turn into property value parts
      (web-capf--push (cons 'pvalue (cdr elem)) syntax))))
   ((memq (car elem) '(string comment))
    (web-capf--push elem syntax))
   ((eq (car elem) 'media)
    (when (web-capf--syntaxp syntax '(mquery nil))
      ;; media query allowed outside property parts
      (web-capf--push elem syntax)))
   ((eq (car elem) 'keyframe)
    (when (web-capf--syntaxp syntax '(mquery nil))
      ;; keyframes allowed outside property parts
      (web-capf--push elem syntax)))))

(defun web-capf--close-syntax-css (syntax key)
  "Close the most recent elem of type KEY on SYNTAX stack,
under the css syntax rules."
  (cond
   ((eq key 'bracket)
    ;; forget attribute value stats
    (web-capf--clean-syntax syntax 'avalue)
    (cond
     ((and (web-capf--syntaxp syntax 'sbracket)
           (web-capf--syntaxp (cdr syntax) '(sparen mquery nil)))
      ;; closed attribute selector: remain as selectors
      ;; NOTE: Remain them to record the selector point found the earliest,
      ;;       so do not update even if found again.
      (setcar (car syntax) 'select))
     ((web-capf--syntaxp syntax '(sbracket pbracket))
      (web-capf--pop syntax))))
   ((eq key 'paren)
    ;; forget selector or attribute value stats
    (web-capf--clean-syntax syntax '(select avalue))
    (when (web-capf--syntaxp syntax '(sparen pparen paren))
      (web-capf--pop syntax)))
   ((eq key 'brace)
    ;; forget selector, property value stats or orphan @-sequences
    (web-capf--clean-syntax syntax '(select pvalue media keyframe))
    (when (web-capf--syntaxp syntax '(mquery kfelem pbrace brace))
      (web-capf--pop syntax)))
   ((eq key 'colon)
    (when (web-capf--syntaxp syntax 'pvalue)
      (web-capf--pop syntax)))))

(defun web-capf--parse-css (&optional beg end)
  "Parse css before END, or current point by default.
Start parsing from BEG if specified."
  (save-excursion
    (let ((syntax (list nil))
          (bound (or end (point))))
      ;; Pass 1: Traverse css from beg to end to analyze syntax.
      (goto-char (or beg (point-min)))
      (catch 'parse
        ;; check keywords successively
        (while (search-forward-regexp web-capf-css-syntax-regexp bound t)
          (let ((piece (match-string-no-properties 0))
                (piece-end (match-end 0)))
            (cond
             ;; brackets
             ((string= piece "[")
              (web-capf--open-syntax-css syntax (cons 'bracket piece-end)))
             ((string= piece "]")
              (web-capf--close-syntax-css syntax 'bracket))
             ;; parens
             ((string= piece "(")
              (web-capf--open-syntax-css syntax (cons 'paren piece-end)))
             ((string= piece ")")
              (web-capf--close-syntax-css syntax 'paren))
             ;; braces
             ((string= piece "{")
              (web-capf--open-syntax-css syntax (cons 'brace piece-end)))
             ((string= piece "}")
              (web-capf--close-syntax-css syntax 'brace))
             ;; sharps or dots
             ((or (string= piece "#") (string= piece "."))
              (web-capf--open-syntax-css syntax (cons 'shpdot piece-end)))
             ;; equals
             ((string= piece "=")
              (web-capf--open-syntax-css syntax (cons 'equal piece-end)))
             ;; colons
             ((string= piece ":")
              (web-capf--open-syntax-css syntax (cons 'colon piece-end)))
             ;; semicolons
             ((string= piece ";")
              (web-capf--close-syntax-css syntax 'colon))
             ;; strings
             ((string= piece "\"")
              (unless (catch 'string
                        ;; search eos
                        (while (search-forward-regexp
                                "\\(\"\\|\\\\\"\\)" bound t)
                          (when (string= (match-string-no-properties 0) "\"")
                            ;; skip whole string if eos found
                            (throw 'string t))))
                ;; check only non-closed string
                (web-capf--open-syntax-css syntax (cons 'string piece-end))
                ;; exit loop cause it always reaches end
                (throw 'parse t)))
             ((string= piece "'")
              (unless (catch 'string
                        ;; search eos
                        (while (search-forward-regexp
                                "\\('\\|\\\\'\\)" bound t)
                          (when (string= (match-string-no-properties 0) "'")
                            ;; skip whole string if eos found
                            (throw 'string t))))
                ;; check only non-closed string
                (web-capf--open-syntax-css syntax (cons 'string piece-end))
                ;; exit loop cause it always reaches end
                (throw 'parse t)))
             ;; comments
             ((string= piece "/*")
              ;; search eoc; skip whole comment if found
              (unless (search-forward "*/" bound t)
                ;; check only non-closed comment
                (web-capf--open-syntax-css syntax (cons 'comment piece-end))
                ;; exit loop cause it always reaches end
                (throw 'parse t)))
             ;; @media sequence
             ((string= piece "@media")
              (web-capf--open-syntax-css syntax (cons 'media piece-end)))
             ;; @keyframes sequence
             ((string= piece "@keyframes")
              (web-capf--open-syntax-css syntax (cons 'keyframe piece-end)))))))
      ;; Pass 2: Decide the part type just before bound.
      (web-capf--clean-syntax syntax '(media keyframe))
      (cond
       ((web-capf--syntaxp syntax '(mquery nil))
        ;; outside property parts
        (cons 'sels-or-global nil))
       ((web-capf--syntaxp syntax 'select)
        ;; after selector keywords outside property parts
        (cons 'sels nil))
       ((web-capf--syntaxp syntax 'sbracket)
        ;; attribute selector parts
        (let ((tag-poses
               ;; earlier selector parts to get tag name
               (mapcar 'cdr (web-capf--clean-syntax
                             syntax '(sbracket sparen select)))))
          (cons 'attr-sel-names tag-poses)))
       ((web-capf--syntaxp syntax 'avalue)
        ;; just after attribute selector name
        (cons 'attr-sel-val-start nil))
       ((web-capf--syntaxp syntax 'string)
        (web-capf--pop syntax)
        (when (web-capf--syntaxp syntax 'avalue)
          ;; inside strings in attribute selector values
          (let ((avalue-pos (cdr (web-capf--pop syntax)))
                (tag-poses
                 ;; earlier selector parts to get tag name
                 (mapcar 'cdr (web-capf--clean-syntax
                               syntax '(sbracket sparen select)))))
            (cons 'attr-sel-vals (cons avalue-pos tag-poses)))))
       ((web-capf--syntaxp syntax 'sparen)
        ;; pseudo selector function arguments
        (or
         ;; get function name and argument types
         (when-let*
             ((match (web-capf--looking-back
                      web-capf-css-sel-funcs-regexp beg (cdar syntax)))
              (func (intern (match-string 2 match)))
              (args (alist-get func web-capf-css-sel-func-args)))
           (when (memq 'web-capf--sels args)
             ;; function takes recursive selectors as arguments
             (cons 'sels nil)))
         ;; otherwise, will complete using selector function arguments table
         (cons 'sel-func-args (cdar syntax))))
       ((web-capf--syntaxp syntax 'kfelem)
        ;; keyframe element parts
        (cons 'keyframe-elems nil))
       ((web-capf--syntaxp syntax 'pbrace)
        ;; inside property parts and not property value parts
        (cons 'prop-names nil))
       ((web-capf--syntaxp syntax 'pvalue)
        ;; property value parts
        (cons 'prop-vals (cdar syntax)))
       ((web-capf--syntaxp syntax 'pparen)
        (let ((pparen-poses
               (mapcar 'cdr (web-capf--clean-syntax syntax 'pparen))))
          (when (web-capf--syntaxp syntax 'pvalue)
            ;; property value function arguments
            (cons 'prop-func-args
                  (append pparen-poses (list (cdar syntax)))))))))))

(defun web-capf--get-css-completions (&optional beg)
  "Return css completion type and keywords at point.
Start parsing from BEG if specified; useful for css part inside html."
  (let ((syntax (web-capf--parse-css beg)))
    (cond
     ((memq (car syntax) '(sels-or-global sels))
      (cond
       ((and (eq (car syntax) 'sels-or-global)
             (web-capf--looking-back web-capf-css-at-keywords-regexp beg))
        ;; at-keywords
        (cons 'at--keyword web-capf-css-at-keywords))
       ((web-capf--looking-back web-capf-css-pseudo-elem-sels-regexp beg)
        ;; pseudo element selectors
        (cons 'pseudo-element-selector web-capf-css-pseudo-elems))
       ((web-capf--looking-back web-capf-css-pseudo-class-sels-regexp beg)
        ;; pseudo class selectors
        (cons 'pseudo-class-selector web-capf-css-pseudo-classes))
       ((web-capf--looking-back web-capf-css-id-or-class-sels-regexp beg)
        ;; id or class selectors: no completions
        nil)
       (t
        ;; element selectors
        (cons 'element-selector
              (mapcar (lambda (elem) (symbol-name (car elem)))
                      web-capf-html-tags-and-attrs)))))
     ((eq (car syntax) 'attr-sel-names)
      ;; attribute selector names
      (let ((attrs
             (catch 'attrs
               (mapc
                (lambda (pos)
                  (when-let* ((match (web-capf--looking-back
                                      web-capf-css-sel-tags-regexp beg pos))
                              (tag (intern (match-string 1 match))))
                    (throw 'attrs
                           (alist-get tag web-capf-html-tags-and-attrs))))
                (cdr syntax))
               (seq-uniq (apply 'append
                                (mapcar 'cdr web-capf-html-tags-and-attrs))))))
        (cons 'attribute-selector-name
              (append attrs web-capf-html-global-attrs))))
     ((eq (car syntax) 'attr-sel-val-start)
      ;; just after attribute selector name: complete only '"'
      (list 'attribute-selector "\""))
     ((eq (car syntax) 'attr-sel-vals)
      (when-let*
          ((match (web-capf--looking-back
                   web-capf-css-attr-sels-regexp beg (cadr syntax)))
           (attr (intern (match-string 2 match))))
        ;; attribute selector values
        (let ((tag
               (catch 'tag
                 (mapc
                  (lambda (pos)
                    (when-let ((match (web-capf--looking-back
                                       web-capf-css-sel-tags-regexp beg pos)))
                      (throw 'tag (intern (match-string 1 match)))))
                  (cddr syntax))
                 nil)))
          (cons 'attribute-selector-value
                (web-capf--get-http-attr-vals tag attr)))))
     ((eq (car syntax) 'sel-func-args)
      (when-let*
          ((match (web-capf--looking-back
                   web-capf-css-sel-funcs-regexp beg (cdr syntax)))
           (func-name (match-string 2 match))
           (func (intern func-name)))
        (when (member (concat func-name "(") web-capf-css-pseudo-classes)
          ;; pseudo selector function arguments
          (cons 'selector-function-arg
                (web-capf--get-css-vals func web-capf-css-sel-func-args)))))
     ((eq (car syntax) 'keyframe-elems)
      ;; keyframe elements
      (cons 'keyframe-element
            (web-capf--get-css-vals (car syntax) web-capf-css-val-classes)))
     ((eq (car syntax) 'prop-names)
      ;; property names
      (cons 'property-name
            (mapcar (lambda (elem) (symbol-name (car elem)))
                    web-capf-css-props-and-vals)))
     ((eq (car syntax) 'prop-vals)
      (cond
       ((web-capf--looking-back web-capf-css-prop-flags-regexp beg)
        ;; property flags
        (cons 'property-flag web-capf-css-global-prop-flags))
       ((when-let*
            ((match (web-capf--looking-back
                     web-capf-css-props-regexp beg (cdr syntax)))
             (prop (intern (match-string 1 match))))
          ;; property values
          (cons 'property-value
                (append
                 (web-capf--get-css-vals prop web-capf-css-props-and-vals)
                 web-capf-css-global-prop-vals
                 web-capf-css-global-prop-funcs))))))
     ((eq (car syntax) 'prop-func-args)
      (let* ((funcp (> (length syntax) 3))
             (regex (if funcp
                        web-capf-css-prop-funcs-regexp
                      web-capf-css-props-regexp))
             (alist (if funcp
                        web-capf-css-prop-func-args
                      web-capf-css-props-and-vals)))
        (when-let*
            ((match1 (web-capf--looking-back regex beg (caddr syntax)))
             (parent (intern (match-string 1 match1)))
             (match2 (web-capf--looking-back
                      web-capf-css-prop-funcs-regexp beg (cadr syntax)))
             (func-name (match-string 1 match2))
             (func (intern func-name)))
          (when (member (concat func-name "(")
                        (web-capf--get-css-vals parent alist))
            ;; property value function arguments
            (cons 'property-function-arg
                  (append
                   (web-capf--get-css-vals func web-capf-css-prop-func-args)
                   web-capf-css-global-prop-funcs)))))))))

(defun web-capf--get-completions ()
  "Return web-related completion type and keywords at point.
The car of return value is the completion type, and rest is keywords.
When the car is t, it means the direction to fallback other capf,
of the language indicated by the cdr."
  (when-let*
      ((compl-type (alist-get major-mode web-capf-compl-types))
       (type (cond
              ((eq compl-type 'complex-web)
               ;; web-mode: get the type which depends on curent point
               (web-capf--get-type-for-web-mode (point)))
              ;; others: return the type determined with major-mode
              (t compl-type))))
    (cond
     ((eq type 'html)
      (web-capf--get-html-completions))
     ((eq type 'css)
      (web-capf--get-css-completions
       ;; limit search area into css part when inside style element of html
       (and (eq major-mode 'web-mode) (web-mode-part-beginning-position))))
     ;; script part: fallback
     (t
      (cons t type)))))

(defun web-capf--get-bounds ()
  "Return bounds to be completed."
  (let ((bounds (or (bounds-of-thing-at-point 'symbol) (cons (point) (point)))))
    (when (eq (char-after (cdr bounds)) ?\()
      (setcdr bounds (1+ (cdr bounds))))
    bounds))

(defun web-capf--get-collection (keywords &optional category)
  "Return completion collection table of CATEGORY, to contain KEYWORDS."
  (if (and keywords category)
      (lambda (str pred action)
        (if (eq action 'metadata)
            `(metadata (category . ,category))
          (complete-with-action action keywords str pred)))
    keywords))

(defun web-capf--get-properties (category)
  "Return completion properties of CATEGORY."
  (let ((category-name
         (concat " " (capitalize (replace-regexp-in-string
                                  "  " "-"
                                  (replace-regexp-in-string
                                   "-" " " (symbol-name category)))))))
    (list :annotation-function (lambda (_) category-name)
          :company-kind (lambda (_) category)
          :exclusive 'no)))

;;;###autoload
(defun web-capf ()
  "Return completion spec at point for web-related modes of
`html-mode', `css-mode' and `web-mode'.
Works as a member of `completion-at-point-functions'."
  (when-let ((completion (web-capf--get-completions)))
    (if (eq (car completion) t)
        ;; fallback
        (let* ((type (symbol-name (cdr completion)))
               (fallback (intern (concat "web-capf-" type "-fallback"))))
          (when (and (boundp fallback) (eval fallback))
            (let ((major-mode (intern (concat type "-mode"))))
              (funcall (eval fallback)))))
      ;; normal completion
      (let ((bounds (web-capf--get-bounds)))
        `(,(car bounds) ,(cdr bounds)
          ,(web-capf--get-collection (cdr completion) (car completion))
          ,@(web-capf--get-properties (car completion)))))))

(provide 'web-capf)
;;; web-capf.el ends here
