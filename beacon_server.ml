open Batteries
open Printf
open Extlib.OptParse
open Lwt

let opt_parser = OptParser.make ~usage:"zcat my.vcf.gz | cut -f1-5 | %prog [options]" ()
let opt ?group ?h ?hide ?s ?short_names ?l ?long_names x = OptParser.add opt_parser ?group ?help:h ?hide ?short_name:s ?long_name:l x; x

let localhost = opt ~l:"localhost" ~h:"bind 127.0.0.1 only" (StdOpt.store_true ())
let port = opt ~l:"port" ~h:"server port (1953)" (StdOpt.int_option ~default:1953 ())
let id = opt ~l:"id" ~h:"beacon ID (narcissome)" (StdOpt.str_option ~default:"narcissome" ())
let organization = opt ~l:"organization" ~h:"beacon organization (narcissus)" (StdOpt.str_option ~default:"narcissus" ())
let description = opt ~l:"description" ~h:"beacon description" (StdOpt.str_option ~default:"" ())
let reference = opt ~l:"reference" ~h:"reference genome assembly (comma-separated synonyms)" (StdOpt.str_option ~default:"GRCh37" ())
let catchall = opt ~l:"catchall" ~h:"catch-all redirect URL instead of 404s" (StdOpt.str_option ())
let qps = opt ~l:"qps" ~h:"enable rate-limiting with this QPS" (StdOpt.float_option ~default:0.0 ())
let backlog = opt ~l:"backlog" ~h:"maximum request backlog during rate-limiting (10)" (StdOpt.int_option ~default:10 ())
let bucket = opt ~l:"bucket" ~h:"variants per bucket for in-memory compression (4000)" (StdOpt.int_option ~default:4000 ())

let cmd = OptParser.parse_argv opt_parser

if Unix.isatty Unix.stdin then
	OptParser.usage opt_parser ()
	exit (-1)

let cfg = {
	Beacon.localhost = Opt.get localhost;
	port = Opt.get port;
	id = Opt.get id;
	organization = Opt.get organization;
	description = Opt.get description;
	reference = String.nsplit ~by:"," (Opt.get reference) |> List.enum |> Set.of_enum;
	catchall = Opt.opt catchall;
	qps = Opt.get qps;
	backlog = Opt.get backlog
}

let data = Beacon.Data.load (Opt.get bucket) stdin
Gc.compact ()

ignore (Lwt_main.run (Beacon.server cfg data))
