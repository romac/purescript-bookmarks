
.PHONY: build watch

build:
	psc-package build

watch:
	fswatch -o src | xargs -n1 -I{} make build

