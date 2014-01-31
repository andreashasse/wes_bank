all: fetch compile xref

fetch:
	./rebar get-deps

xref:
	./rebar skip_deps=true xref

compile:
	./rebar compile

app:
	./rebar skip_deps=true compile xref

eunit:
	./rebar skip_deps=true compile xref verbose=1 eunit

init_dialyzer:
	dialyzer --apps stdlib kernel erts -r deps --build_plt --output_plt .dialyzer.plt

check: compile
	dialyzer --no_native -Wno_undefined_callbacks -Wno_return -r ebin --plt .dialyzer.plt

start:
	erl -pa deps/*/ebin ebin -eval "app_util:dev_start(wes_bank, permanent)." -name bank@localhost -config priv/development
