all: doc
.PHONY: doc install check

doc:
	R -e 'devtools::document()'

install:
	R -e "remotes::install_github('GuillaumeHevin/TooffR_package')"

check:
	R -e 'devtools::check()'

