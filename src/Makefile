SHELL := /bin/bash
.PHONY: all clean
.DEFAULT_GOAL := all


OUTDIR=./

all: protobuffs_scanner.erl protobuffs_parser.erl pokemon_pb.beam erl game_pb.beam 

pokemon_pb.beam:pokemon_pb.erl
	erlc +debug_info -o $(OUTDIR) $^

protobuffs_scanner.erl:protobuffs_scanner.xrl
	erlc -o $(OUTDIR) $^
	
protobuffs_parser.erl:protobuffs_parser.yrl
	erlc -o $(OUTDIR) $^
	
RCS=$(notdir $(wildcard *.erl))
BEAMS=$(patsubst %.erl, %.beam,$(RCS))

erl:$(BEAMS)
	
%.beam:%.erl
	erlc -o $(OUTDIR) $^
	
game_pb.beam:game.proto
	@erl -noinput -eval "protobuffs_compile:scan_file(\"game.proto\"),halt(0)"
	

clean:
	rm -f *.beam && rm -f protobuffs_scanner.erl protobuffs_parser.erl game_pb_map.erl
