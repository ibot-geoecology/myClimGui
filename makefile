.PHONY: install, build, remove, generate, generate-source, generate-documentation, check

install: build
	R -e 'install.packages("../myClimGui_latest.tar.gz", repos=NULL)'

build:
	R -e 'devtools::build(".", path="../myClimGui_latest.tar.gz")'

remove:
	R -e 'remove.packages("myClimGui")'

generate: generate-source generate-documentation

generate-source:
	$(RM) NAMESPACE

generate-documentation:
	R -e 'devtools::document()'

check:
	R --vanilla --no-multiarch -e 'devtools::check()'
