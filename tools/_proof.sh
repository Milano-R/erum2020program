#!/bin/bash

args=(

  # external link cache, set to 28d for re-checking existing links possibly
  # turning broken every 4 weeks
  --timeframe 28d

  # ignored external URLs:
  #  - Linkedin links are known to have `999 No error` (gjtorikian/html-proofer#215) and are excluded
  --url-ignore /linkedin.com/

  # enables HTML validation errors from Nokogumbo
  --check-html
  # full-set of check-html errors to be reported
  --report-invalid-tags
  --report-missing-names
  --report-script-embeds
  --report-missing-doctype
  --report-eof-tags
  --report-mismatched-tags

)

htmlproofer "${args[@]}" "$@"
