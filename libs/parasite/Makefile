all: pull build
deploy: pull build mindist push

pull:
	jx pull -f

build:
	jx build -f

mindist:
	jx mindist -f

push:
	jx push -f
