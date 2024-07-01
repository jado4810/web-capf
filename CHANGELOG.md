# web-capf.el - Changelog

## Version 0.9 (2024-07-xx)

* New css features
    * Support css nesting
    * Support container query and `container\*` properties
* Add inline mathml tag hierarchy, attribute and attribute value rules
* Fix some issues on html completion
    * Recognize self-closing svg (and mathml) with attributes
    * Completion after inline script blocks: `<?`\~`?>` or  `<%`\~`%>`

## Version 0.5 (2024-06-10)

* Add inline svg tag hierarchy, attribute and attribute value rules

## Version 0.41 (2023-09-28)

* Fix broken hierarchy rules

## Version 0.4 (2023-08-01)

* Completion according to html tag hierarchy rules

## Version 0.3 (2023-05-21)

* Add `!important` completion

## Version 0.2 (2022-12-22)

* Add `DOCTYPE html` completion
* Fix broken completions on html with comments
* Fix problem that did not work when byte compiled with batch-byte-compile

## Version 0.1 (2022-11-20)

* Initial version
