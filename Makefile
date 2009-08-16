LIBDIR					= `erl -eval 'io:format("~s~n", [code:lib_dir()])' -s init stop -noshell`
VERSION					= 0.0.1
CC							= erlc
ERL							= erl
EBIN						= ebin
CFLAGS					= +debug_info -W0 -I include -pa $(EBIN)
COMPILE					= $(CC) $(CFLAGS) -o $(EBIN)
EBIN_DIRS				= $(wildcard deps/*/ebin)

all: ebin compile
	
compile:
	@$(ERL) -pa $(EBIN_DIRS) -pa $(EBIN) -noinput +B -eval 'case make:all() of up_to_date -> halt(0); error -> halt(1) end.'

eunit:
	cd test/include/eunit && make
	
test: compile
	$(ERL) -noshell -pa $(EBIN) -pa test/ebin -s test_suite test -s init stop
	
ebin:
	@mkdir ebin

clean:
	rm -rf ebin/*.beam ebin/erl_crash.dump erl_crash.dump ebin/*.boot ebin/*.rel ebin/*.script test/ebin/*.beam
