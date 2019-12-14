.PHONY: all clean build test ghcid hoogle

PACKAGE = bind-torrent

STACK = stack
STACK_BUILD = build --pedantic
STACK_TEST = --test --work-dir=.stack-work-test

all: build test

build:
	$(STACK) $(STACK_BUILD)

test:
	$(STACK) $(STACK_BUILD) $(STACK_TEST) \
		--coverage

clean:
	$(STACK) clean --full
	$(STACK_TEST) clean --full

ghcid:
	$(STACK) exec -- ghcid -c "stack ghci $(PACKAGE) --test --ghci-options='-fobject-code -fno-warn-unused-do-bind' --main-is $(PACKAGE):bind-torrent-exe"

hoogle:
	$(STACK) hoogle server -- --local --port 8000

