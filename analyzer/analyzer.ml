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
  if !Options.verbose then Format.printf "%a" ControlFlowGraphPrinter.print_cfg cfg ;
  ControlFlowGraphPrinter.output_dot !Options.cfg_out cfg ;
  let module PartialIterator = Iterator.Make (Domains.Domain.MakeForwardOnly) in
  match !Options.domain with
  | "sign" ->
          let module SignIterator =
              PartialIterator (Domains.Sign)
          in
          SignIterator.iterate cfg
  | "interval" ->
          let module IntervalIterator =
              PartialIterator (Domains.Interval)
          in
          IntervalIterator.iterate cfg
  | "congruence" ->
          let module CongruenceIterator =
              PartialIterator (Domains.Congruence)
          in
          CongruenceIterator.iterate cfg
  | "reducedProduct" ->
          let module ReducedProductIterator =
              PartialIterator (Domains.ReducedProduct)
          in
          ReducedProductIterator.iterate cfg
  | "polyhedral" ->
          let module PolyhedralIterator = 
              PartialIterator (Domains.Polyhedral)
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
