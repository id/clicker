REBAR ?= $(or $(shell which rebar3 2>/dev/null),$(CURDIR)/rebar3)
REBAR_URL ?= https://s3.amazonaws.com/rebar3/rebar3
CT_NODE_NAME ?= clicker@127.0.0.1

.PHONY: all
all: compile

.PHONY: compile
compile: $(REBAR)
	@$(REBAR) compile

.PHONY: xref
xref: $(REBAR)
	$(REBAR) xref

.PHONY: eunit
eunit: compile
	$(REBAR) eunit verbose=true

.PHONY: ct
ct: compile
	$(REBAR) ct -v --name $(CT_NODE_NAME)

.PHONY: cover
cover: $(REBAR)
	$(REBAR) cover

.PHONY: test
test: eunit ct cover

.PHONY: test-env-up
test-env-up:
	docker-compose --file .ci/docker-compose.yml up -d

.PHONY: test-env-down
test-env-down:
	@docker-compose --file ./.ci/docker-compose.yml down

.PHONY: dialyzer
dialyzer: $(REBAR)
	$(REBAR) dialyzer

.PHONY: shell
shell: $(REBAR)
	@$(REBAR) shell

.PHONY: fmt
fmt: $(REBAR)
	@$(REBAR) fmt

$(REBAR):
	@curl -skfL "$(REBAR_URL)" -o $@
	@chmod +x $@
