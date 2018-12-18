PWD   := $(shell pwd)
SCP   := $(shell which scp)
CP    := $(shell which cp)
MV    := $(shell which mv)
MKDIR := $(shell which mkdir)
RM    := $(shell which rm)
SED   := $(shell which sed)
VER   := $(shell cat ./Version)
TMP   := /tmp
TAR   := $(shell which tar)
FS    := username@file.server.address:~/path.in.home

.PHONY: proto compile        shell test	    console-dev		rel-dev rel-stage rel-prod

all: proto compile

proto:
	$(PWD)/script/gpb -pkgs 	-I $(PWD)/proto 	-o-erl $(PWD)/src 	-o-hrl $(PWD)/include 	$(PWD)/proto/*.proto

compile:
	$(PWD)/script/rebar3 compile

shell:
	$(PWD)/script/rebar3 shell

test:
	$(PWD)/script/rebar3 ct

console-dev:
	_build/dev/rel/rtmp/bin/rtmp console

rel-dev:
	$(PWD)/script/rebar3 as dev release

rel-prod:
	$(SED) -i 's/{rtmp, "rtmp-version"}/{rtmp, "$(VER)"}/g' ./rebar.config
	$(PWD)/script/rebar3 as prod release
	$(PWD)/script/rebar3 as prod tar
	$(SED) -i 's/{rtmp, "$(VER)"}/{rtmp, "rtmp-version"}/g' ./rebar.config
    #$(SCP) -P 8522 $(PWD)/_build/prod/rel/rtmp/rtmp-$(VER).tar.gz $(FS)
	@printf "\nApplication: %s\n" $(PWD)/_build/prod/rel/rtmp/rtmp-$(VER).tar.gz


rel-stage:
	$(SED) -i 's/{rtmp, "rtmp-version"}/{rtmp, "$(VER)"}/g' ./rebar.config
	$(PWD)/script/rebar3 as stage release
	$(PWD)/script/rebar3 as stage tar
	$(SED) -i 's/{rtmp, "$(VER)"}/{rtmp, "rtmp-version"}/g' ./rebar.config
    #$(SCP) -P 8522 $(PWD)/_build/stage/rel/rtmp/rtmp-$(VER).tar.gz $(FS)
	@printf "\nApplication: %s\n" $(PWD)/_build/stage/rel/rtmp/rtmp-$(VER).tar.gz
