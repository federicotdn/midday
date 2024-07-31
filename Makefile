SHELL = bash

run:
	emacs -q -l midday.el \
	  --eval '(setq midday-debug t)' \
	  --eval '(setq midday-predicates (midday-standard-predicates))'
