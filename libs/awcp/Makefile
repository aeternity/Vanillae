all: prepare build

deploy: prepare build mindist push

prepare:
	jx pull -f

build:
	jx build -f

mindist:
	jx mindist -f

push:
	jx push -f
