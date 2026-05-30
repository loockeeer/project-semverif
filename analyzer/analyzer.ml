(*
  Cours "Sémantique et Application à la Vérification de programmes"

  Ecole normale supérieure, Paris, France / CNRS / INRIA
*)

(*
  Simple driver: parses the file given as argument and prints it back.

  You should modify this file to call your functions instead!
*)

open Frontend

(* parse filename *)
let doit filename =
  let prog = FileParser.parse_file filename in
  let cfg = Tree_to_cfg.prog prog in
  (if !Options.verbose then 
      Format.printf "%a" ControlFlowGraphPrinter.print_cfg cfg);
  ControlFlowGraphPrinter.output_dot !Options.cfg_out cfg;
  let module Vars = struct let support = cfg.cfg_vars end in
  let module PartialDomain = Domains.Domain.MakeForwardOnly (Vars) in
  match !Options.domain with
  | "constants" ->
          let module D =
              PartialDomain (Domains.Integer)
          in
          let module IntegersIterator = Iterator.Make(D) in
          IntegersIterator.iterate cfg
  | "sign" ->
          let module D =
              PartialDomain (Domains.Sign)
          in
          let module SignIterator = Iterator.Make(D) in
          SignIterator.iterate cfg
  | "interval" ->
          let module D =
              PartialDomain (Domains.Interval)
          in
          let module IntervalIterator = Iterator.Make(D) in
          IntervalIterator.iterate cfg
  | "congruence" ->
          let module D =
              PartialDomain (Domains.Congruence)
          in
          let module CongruenceIterator = Iterator.Make(D) in
          CongruenceIterator.iterate cfg
  | "reducedProduct" ->
          let module D =
              PartialDomain (Domains.ReducedProduct)
          in
          let module ReducedProductIterator = Iterator.Make(D) in
          ReducedProductIterator.iterate cfg
  | "polyhedral" ->
          let module PolyhedralIterator =
              Iterator.Make(Domains.Polyhedral.Make (Vars) (Domains.Integer))
          in
          PolyhedralIterator.iterate cfg
  | s -> (
        Printf.printf "error: the value domain %s does not exist\n" s;
        print_endline "available value domains: sign, interval, congruence, reducedProduct, polyhedral";
        exit 1
        )
(* parses arguments to get filename *)
let main () =
  let _ = Options.init () in
  doit !Options.file

let _ = main ()
