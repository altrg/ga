NODE=ga
CFG=ga.cfg

APP=ga_app
ERL=@ERL_LIBS=deps erl +K true -pa ebin
RE_LOG_MAXSIZE=10000000
RE_DIR=log/run_erl/
REBAR=@./rebar

.PHONY: all deps

all:
	$(REBAR) compile skip_deps=true

deps:
	$(REBAR) get-deps compile

clean:
	$(REBAR) clean
	@rm -rf erl_crash.dump

run:
	$(ERL) -s $(APP) -sname $(NODE) -cfg $(CFG)

start:
	@echo -n "Starting $(NODE).."
	@mkdir -p $(RE_DIR)
	@run_erl -daemon $(RE_DIR) $(RE_DIR) "exec make run"
	@echo "ok"

attach:
	@to_erl $(RE_DIR)

stop:
	@echo -n "Stopping $(NODE).."
	@echo "init:stop()." | to_erl $(RE_DIR) 2>/dev/null
	@echo "ok"
