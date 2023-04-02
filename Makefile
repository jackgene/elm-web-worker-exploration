.PHONY: default
default: all

.PHONY: all
all: fibonacci-no-worker

.PHONY: fibonacci-no-worker
fibonacci-no-worker:
	cd fibonacci-no-worker &&\
	elm make --optimize src/Main.elm --output=app/index.html
