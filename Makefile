REBAR ?= $(CURDIR)/rebar3
REBAR_URL ?= https://s3.amazonaws.com/rebar3/rebar3
CT_NODE_NAME ?= clicker@127.0.0.1

all: compile

compile: $(REBAR)
	@$(REBAR) compile

xref: $(REBAR)
	$(REBAR) xref

eunit: compile
	$(REBAR) eunit verbose=true

ct: compile
	$(REBAR) ct -v --name $(CT_NODE_NAME)

cover: $(REBAR)
	$(REBAR) cover

test: eunit ct cover

dialyzer: $(REBAR)
	$(REBAR) dialyzer

shell: $(REBAR)
	@$(REBAR) shell

fmt: $(REBAR)
	@$(REBAR) fmt

$(REBAR):
	@curl -skfL "$(REBAR_URL)" -o ./rebar3
	@chmod +x ./rebar3
