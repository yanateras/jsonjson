docs: README.edoc src/json.erl
	erl -noshell -run edoc_run files '["src/json.erl"]' \
		'[{dir, "$@"}, {overview, "README.edoc"}, {sort_functions, false}]'

%.html: %.org
	emacs $< --batch --eval '(org-html-export-to-html nil nil nil t)'

%.edoc: %.html
	(echo @doc; cat $<) > $@
