open Batteries
open Printf
open Extlib.OptParse
open Lwt

let opt_parser = OptParser.make ~usage:"zcat my.vcf.gz | cut -f1-5 | %prog [options]" ()
let opt ?group ?h ?hide ?s ?short_names ?l ?long_names x = OptParser.add opt_parser ?group ?help:h ?hide ?short_name:s ?long_name:l x; x

let port = opt ~l:"port" ~h:"server port (1953)" (StdOpt.int_option ~default:1953 ())
let id = opt ~l:"id" ~h:"beacon ID (narcissome)" (StdOpt.str_option ~default:"narcissome" ())
let organization = opt ~l:"org" ~h:"beacon organization (narcissus)" (StdOpt.str_option ~default:"narcissus" ())
let description = opt ~l:"desc" ~h:"beacon description" (StdOpt.str_option ~default:"" ())

let cmd = OptParser.parse_argv opt_parser

if Unix.isatty Unix.stdin then
	OptParser.usage opt_parser ()
	exit (-1)

let cfg = {
	Beacon.port = Opt.get port;
	id = Opt.get id;
	organization = Opt.get organization;
	description = Opt.get description
}

ignore (Lwt_main.run (Beacon.server cfg (Beacon.Data.load stdin)))
