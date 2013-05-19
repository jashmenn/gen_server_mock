REBAR=./rebar

.PHONY: all
all: compile

compile:
	@$(REBAR) compile

test: force
	@$(REBAR) eunit

clean:
	@$(REBAR) clean

force: ;
