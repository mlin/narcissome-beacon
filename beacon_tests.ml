open Printf
open Lwt
open Cohttp
open Cohttp_lwt_unix
open JSON.Operators
open Should
open Batteries

let (@) = JSON.Operators.($)

let test_vcf = {testvcf|##fileformat=VCFv4.1
##fileDate=20140404
##source=freeBayes v0.9.14
##reference=genome.fa
##phasing=none
##commandline="freebayes -b input-0.bam -v 1.vcf -f genome.fa -r 1"
##filter="QUAL > 29.999999 SAF > 1 SAR > 1 AB < 0.800001 DP < 59"
##INFO=<ID=NS,Number=1,Type=Integer,Description="Number of samples with data">
##INFO=<ID=DP,Number=1,Type=Integer,Description="Total read depth at the locus">
##INFO=<ID=DPB,Number=1,Type=Float,Description="Total read depth per bp at the locus; bases in reads overlapping / bases in haplotype">
##INFO=<ID=AC,Number=A,Type=Integer,Description="Total number of alternate alleles in called genotypes">
##INFO=<ID=AN,Number=1,Type=Integer,Description="Total number of alleles in called genotypes">
##INFO=<ID=AF,Number=A,Type=Float,Description="Estimated allele frequency in the range (0,1]">
##INFO=<ID=RO,Number=1,Type=Integer,Description="Reference allele observation count, with partial observations recorded fractionally">
##INFO=<ID=AO,Number=A,Type=Integer,Description="Alternate allele observations, with partial observations recorded fractionally">
##INFO=<ID=PRO,Number=1,Type=Float,Description="Reference allele observation count, with partial observations recorded fractionally">
##INFO=<ID=PAO,Number=A,Type=Float,Description="Alternate allele observations, with partial observations recorded fractionally">
##INFO=<ID=QR,Number=1,Type=Integer,Description="Reference allele quality sum in phred">
##INFO=<ID=QA,Number=A,Type=Integer,Description="Alternate allele quality sum in phred">
##INFO=<ID=PQR,Number=1,Type=Float,Description="Reference allele quality sum in phred for partial observations">
##INFO=<ID=PQA,Number=A,Type=Float,Description="Alternate allele quality sum in phred for partial observations">
##INFO=<ID=SRF,Number=1,Type=Integer,Description="Number of reference observations on the forward strand">
##INFO=<ID=SRR,Number=1,Type=Integer,Description="Number of reference observations on the reverse strand">
##INFO=<ID=SAF,Number=A,Type=Integer,Description="Number of alternate observations on the forward strand">
##INFO=<ID=SAR,Number=A,Type=Integer,Description="Number of alternate observations on the reverse strand">
##INFO=<ID=SRP,Number=1,Type=Float,Description="Strand balance probability for the reference allele: Phred-scaled upper-bounds estimate of the probability of observing the deviation between SRF and SRR given E(SRF/SRR) ~ 0.5, derived using Hoeffding's inequality">
##INFO=<ID=SAP,Number=A,Type=Float,Description="Strand balance probability for the alternate allele: Phred-scaled upper-bounds estimate of the probability of observing the deviation between SAF and SAR given E(SAF/SAR) ~ 0.5, derived using Hoeffding's inequality">
##INFO=<ID=AB,Number=A,Type=Float,Description="Allele balance at heterozygous sites: a number between 0 and 1 representing the ratio of reads showing the reference allele to all reads, considering only reads from individuals called as heterozygous">
##INFO=<ID=ABP,Number=A,Type=Float,Description="Allele balance probability at heterozygous sites: Phred-scaled upper-bounds estimate of the probability of observing the deviation between ABR and ABA given E(ABR/ABA) ~ 0.5, derived using Hoeffding's inequality">
##INFO=<ID=RUN,Number=A,Type=Integer,Description="Run length: the number of consecutive repeats of the alternate allele in the reference genome">
##INFO=<ID=RPP,Number=A,Type=Float,Description="Read Placement Probability: Phred-scaled upper-bounds estimate of the probability of observing the deviation between RPL and RPR given E(RPL/RPR) ~ 0.5, derived using Hoeffding's inequality">
##INFO=<ID=RPPR,Number=1,Type=Float,Description="Read Placement Probability for reference observations: Phred-scaled upper-bounds estimate of the probability of observing the deviation between RPL and RPR given E(RPL/RPR) ~ 0.5, derived using Hoeffding's inequality">
##INFO=<ID=EPP,Number=A,Type=Float,Description="End Placement Probability: Phred-scaled upper-bounds estimate of the probability of observing the deviation between EL and ER given E(EL/ER) ~ 0.5, derived using Hoeffding's inequality">
##INFO=<ID=EPPR,Number=1,Type=Float,Description="End Placement Probability for reference observations: Phred-scaled upper-bounds estimate of the probability of observing the deviation between EL and ER given E(EL/ER) ~ 0.5, derived using Hoeffding's inequality">
##INFO=<ID=DPRA,Number=A,Type=Float,Description="Alternate allele depth ratio.  Ratio between depth in samples with each called alternate allele and those without.">
##INFO=<ID=ODDS,Number=1,Type=Float,Description="The log odds ratio of the best genotype combination to the second-best.">
##INFO=<ID=GTI,Number=1,Type=Integer,Description="Number of genotyping iterations required to reach convergence or bailout.">
##INFO=<ID=TYPE,Number=A,Type=String,Description="The type of allele, either snp, mnp, ins, del, or complex.">
##INFO=<ID=CIGAR,Number=A,Type=String,Description="The extended CIGAR representation of each alternate allele, with the exception that '=' is replaced by 'M' to ease VCF parsing.  Note that INDEL alleles do not have the first matched base (which is provided by default, per the spec) referred to by the CIGAR.">
##INFO=<ID=NUMALT,Number=1,Type=Integer,Description="Number of unique non-reference alleles in called genotypes at this position.">
##INFO=<ID=MEANALT,Number=A,Type=Float,Description="Mean number of unique non-reference allele observations per sample with the corresponding alternate alleles.">
##INFO=<ID=LEN,Number=A,Type=Integer,Description="allele length">
##INFO=<ID=MQM,Number=A,Type=Float,Description="Mean mapping quality of observed alternate alleles">
##INFO=<ID=MQMR,Number=1,Type=Float,Description="Mean mapping quality of observed reference alleles">
##INFO=<ID=PAIRED,Number=A,Type=Float,Description="Proportion of observed alternate alleles which are supported by properly paired read fragments">
##INFO=<ID=PAIREDR,Number=1,Type=Float,Description="Proportion of observed reference alleles which are supported by properly paired read fragments">
##INFO=<ID=technology.ILLUMINA,Number=A,Type=Float,Description="Fraction of observations supporting the alternate observed in reads from ILLUMINA">
##FORMAT=<ID=GT,Number=1,Type=String,Description="Genotype">
##FORMAT=<ID=GQ,Number=1,Type=Float,Description="Genotype Quality, the Phred-scaled marginal (or unconditional) probability of the called genotype">
##FORMAT=<ID=GL,Number=G,Type=Float,Description="Genotype Likelihood, log10-scaled likelihoods of the data given the called genotype for each possible genotype generated from the reference and alternate alleles given the sample ploidy">
##FORMAT=<ID=DP,Number=1,Type=Integer,Description="Read Depth">
##FORMAT=<ID=RO,Number=1,Type=Integer,Description="Reference allele observation count">
##FORMAT=<ID=QR,Number=1,Type=Integer,Description="Sum of quality of the reference observations">
##FORMAT=<ID=AO,Number=A,Type=Integer,Description="Alternate allele observation count">
##FORMAT=<ID=QA,Number=A,Type=Integer,Description="Sum of quality of the alternate observations">
##INFO=<ID=CSQ,Number=.,Type=String,Description="Consequence type as predicted by VEP. Format: Allele|Gene|Feature|Feature_type|Consequence|cDNA_position|CDS_position|Protein_position|Amino_acids|Codons|Existing_variation|AA_MAF|EA_MAF|EXON|INTRON|MOTIF_NAME|MOTIF_POS|HIGH_INF_POS|MOTIF_SCORE_CHANGE|DISTANCE|STRAND|CLIN_SIG|CANONICAL|SYMBOL|SYMBOL_SOURCE|SIFT|PolyPhen|GMAF|BIOTYPE|ENSP|DOMAINS|CCDS|AFR_MAF|AMR_MAF|ASN_MAF|EUR_MAF|PUBMED">
#CHROM	POS	ID	REF	ALT	QUAL	FILTER	INFO	FORMAT	PG0001312-BLD
12	112241766	.	G	A	1264	.	AB=0;ABP=0;AC=2;AF=1;AN=2;AO=42;CIGAR=1X;DP=42;DPB=42;DPRA=0;EPP=23.691;EPPR=0;GTI=0;LEN=1;MEANALT=1;MQM=60;MQMR=0;NS=1;NUMALT=1;ODDS=58.5772;PAIRED=1;PAIREDR=0;PAO=0;PQA=0;PQR=0;PRO=0;QA=1467;QR=0;RO=0;RPP=4.87156;RPPR=0;RUN=1;SAF=27;SAP=10.4553;SAR=15;SRF=0;SRP=0;SRR=0;TYPE=snp;technology.ILLUMINA=1;CSQ=A|ENSG00000111275|ENST00000549106|Transcript|3_prime_UTR_variant&NMD_transcript_variant|441|||||rs671&CM870003&COSM147611|||3/4|||||||1|drug-response&other||ALDH2|HGNC|||A:0.0574|nonsense_mediated_decay|ENSP00000474669|||0|0.0028|0.22|0|19041386&22171074&18996923&20139978&20616999&18317873&18331377&19262484&19641380&19698717&19706845&20093384&20417517&20518787&20833657&21351086&21372407&21437268&21455501&21576033&21900886&21917409&21946912&21971053&22004425&22102315&22286173&22508505&22551939&22560290&22930414&23088731&23243119&23364009&23430454&23455379&23697560,A|ENSG00000111275|ENST00000416293|Transcript|missense_variant|1465|1369|457|E/K|Gaa/Aaa|rs671&CM870003&COSM147611|||11/12|||||||1|drug-response&other||ALDH2|HGNC|deleterious(0.01)|probably_damaging(0.955)|A:0.0574|protein_coding|ENSP00000403349|Pfam_domain:PF00171&Superfamily_domains:SSF53720|CCDS55885.1|0|0.0028|0.22|0|19041386&22171074&18996923&20139978&20616999&18317873&18331377&19262484&19641380&19698717&19706845&20093384&20417517&20518787&20833657&21351086&21372407&21437268&21455501&21576033&21900886&21917409&21946912&21971053&22004425&22102315&22286173&22508505&22551939&22560290&22930414&23088731&23243119&23364009&23430454&23455379&23697560,A|ENSG00000111275|ENST00000548536|Transcript|3_prime_UTR_variant&NMD_transcript_variant|1661|||||rs671&CM870003&COSM147611|||13/14|||||||1|drug-response&other||ALDH2|HGNC|||A:0.0574|nonsense_mediated_decay|ENSP00000448179|||0|0.0028|0.22|0|19041386&22171074&18996923&20139978&20616999&18317873&18331377&19262484&19641380&19698717&19706845&20093384&20417517&20518787&20833657&21351086&21372407&21437268&21455501&21576033&21900886&21917409&21946912&21971053&22004425&22102315&22286173&22508505&22551939&22560290&22930414&23088731&23243119&23364009&23430454&23455379&23697560,A|ENSG00000111275|ENST00000261733|Transcript|missense_variant|1571|1510|504|E/K|Gaa/Aaa|rs671&CM870003&COSM147611|||12/13|||||||1|drug-response&other|YES|ALDH2|HGNC|deleterious(0.01)|probably_damaging(0.91)|A:0.0574|protein_coding|ENSP00000261733|Pfam_domain:PF00171&Superfamily_domains:SSF53720|CCDS9155.1|0|0.0028|0.22|0|19041386&22171074&18996923&20139978&20616999&18317873&18331377&19262484&19641380&19698717&19706845&20093384&20417517&20518787&20833657&21351086&21372407&21437268&21455501&21576033&21900886&21917409&21946912&21971053&22004425&22102315&22286173&22508505&22551939&22560290&22930414&23088731&23243119&23364009&23430454&23455379&23697560	GT:DP:RO:QR:AO:QA:GL	1/1:42:0:0:42:1467:-10,-10,0
1	4501983	.	CA	TG,CG	1124.96	.	AB=0.410256,0.589744;ABP=5.73856,5.73856;AC=1,1;AF=0.5,0.5;AN=2;AO=16,23;CIGAR=2X,1M1X;DP=39;DPB=39;DPRA=0,0;EPP=5.18177,3.86001;EPPR=0;GTI=0;LEN=2,1;MEANALT=2,2;MQM=60,60;MQMR=0;NS=1;NUMALT=2;ODDS=82.9187;PAIRED=1,1;PAIREDR=0;PAO=0,0;PQA=0,0;PQR=0;PRO=0;QA=587,886;QR=0;RO=0;RPP=3.0103,5.3706;RPPR=0;RUN=1,1;SAF=4,7;SAP=11.6962,10.6577;SAR=12,16;SRF=0;SRP=0;SRR=0;TYPE=mnp,snp;technology.ILLUMINA=1,1;CSQ=TG||||intergenic_variant||||||||||||||||||||||||||||||||,CG||||intergenic_variant||||||||||||||||||||||||||||||||	GT:DP:RO:QR:AO:QA:GL	1/2:39:0:0:16,23:587,886:-10,-10,-10,-10,0,-10
10	123456790	.	A	G,GG
10	123456790	.	AA	G,GG
|testvcf}

let test_config = {
  Beacon.localhost = true;
  port = 8222;
  id = "test";
  organization = "test";
  description = "test";
  reference = Set.add "GRCh37" Set.empty;
  catchall = None;
  qps = 10.0;
  backlog = 10
}

let test_data = Beacon.Data.load 1 (IO.input_string test_vcf)

let get path = Client.get (Uri.of_string (sprintf "http://127.0.0.1:%d%s" test_config.Beacon.port path))
let getbody path =
  let%lwt (response,body) = get path
  let%lwt body = Cohttp_lwt_body.to_string body
  Lwt.return (response,JSON.from_string body)

let ok path exists =
  let%lwt (response,body) = getbody path
  Response.status response $hould # equal `OK
  ((body@"response")@"exists") $hould # equal (`String exists)
  Lwt.return ()

let tests () =
  (* start the server *)
  Lwt.async
    fun _ ->
      try%lwt Beacon.server test_config test_data
      with exn ->
        eprintf "%s\n" (Printexc.to_string exn)
        Lwt.fail exn
  let%lwt _ = Lwt_unix.sleep 0.1

  let%lwt (response,body) = get "/beacon"
  Response.status response $hould # equal `Not_found

  let%lwt (response,body) = getbody "/beacon/info"
  Response.status response $hould # equal `OK
  printf "%s\n" (JSON.to_string body)

  let%lwt (response,body) = get "/beacon/query"
  Response.status response $hould # equal `Bad_request

  let%lwt _ = ok "/beacon/query?chromosome=12&position=112241765&alternateBases=A" "True"
  let%lwt _ = ok "/beacon/query?chromosome=12&position=112241765&alternateBases=A&referenceBases=G" "True"
  let%lwt _ = ok "/beacon/query?chromosome=1&position=4501982&alternateBases=CG" "True"
  let%lwt _ = ok "/beacon/query?chromosome=10&position=123456789&alternateBases=G&referenceBases=AA" "True"
  let%lwt _ = ok "/beacon/query?chromosome=10&position=123456789&alternateBases=G" "True"

  let%lwt _ = ok "/beacon/query?chromosome=12&position=112241765&alternateBases=A&referenceBases=T" "Overlap"
  let%lwt _ = ok "/beacon/query?chromosome=12&position=112241765&alternateBases=T" "Overlap"
  let%lwt _ = ok "/beacon/query?chromosome=1&position=4501982&alternateBases=C" "Overlap"
  let%lwt _ = ok "/beacon/query?chromosome=10&position=123456789&alternateBases=C" "Overlap"
  let%lwt _ = ok "/beacon/query?chromosome=10&position=123456789&alternateBases=G&referenceBases=T" "Overlap"

  let%lwt _ = ok "/beacon/query?chromosome=12&position=12345678&alternateBases=A" "False"
  let%lwt _ = ok "/beacon/query?chromosome=11&position=112241765&alternateBases=A" "Null"

  (* rate limiting *)
  let t0 = Unix.gettimeofday ()
  let%lwt responses = List.of_enum (1 -- 12) |> Lwt_list.map_p (fun _ -> getbody "/beacon/query?chromosome=12&position=112241765&alternateBases=A")
  let t1 = Unix.gettimeofday ()
  (t1 -. t0) $hould # be # at # least 1.0
  let accepted, rejected =
    responses |> List.partition (function (rsp,_) when Response.status rsp = `OK -> true | _ -> false)
  List.length accepted $hould # be # at # least 10
  list rejected $houldn't # be # empty
  rejected |> List.iter (function (rsp,_) -> Response.status rsp $hould # equal `Too_many_requests)

  let%lwt _ = Lwt_unix.sleep 0.2
  let t0 = Unix.gettimeofday ()
  let%lwt _ = ok "/beacon/query?chromosome=12&position=112241765&alternateBases=A" "True"
  let t1 = Unix.gettimeofday ()
  (t1 -. t0) $hould # be # below 0.01

  Lwt.return ()

 (* TODO: larger test with some NA12878 vcf *)

Lwt_main.run (tests ())
