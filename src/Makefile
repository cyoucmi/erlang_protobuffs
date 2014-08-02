SHELL := /bin/bash
.PHONY: all clean
.DEFAULT_GOAL := all

RCS=$(notdir $(wildcard *.erl))
BEAMS=$(patsubst %.erl, %.beam,$(RCS))
OUTDIR=./

all: protobuffs_scanner.erl protobuffs_parser.erl $(BEAMS) game_pb.beam

pokemon_pb.beam:pokemon_pb.erl
	erlc +debug_info -o $(OUTDIR) $^

protobuffs_scanner.erl:protobuffs_scanner.xrl
	erlc -o $(OUTDIR) $^
	
protobuffs_parser.erl:protobuffs_parser.yrl
	erlc -o $(OUTDIR) $^
	
%.beam:%.erl
	erlc -o $(OUTDIR) $^
	
game_pb.beam:game.proto
	@erl -noshell -eval "protobuffs_compile:scan_file(\"game.proto\")"
	

clean:
	rm -f *.beam && rm -f protobuffs_scanner.erl protobuffs_parser.erl
