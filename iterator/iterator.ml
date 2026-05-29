(*
  Cours "Sémantique et Application à la Vérification de programmes"

  Ecole normale supérieure, Paris, France / CNRS / INRIA
*)

open Frontend
open ControlFlowGraph
open Domains

module Make (DM : Domain.DOMAIN_MAKE) (VD: ValueDomain.VALUE_DOMAIN) = 
    struct
    module Dom = DM(VD)
    (* [find_loop_nodes] returns a [NodeSet] of nodes belonging to a cycle.
       They should be interpreted as the nodes on which a loop loops *)
    let find_loop_nodes cfg =
        let visited = Hashtbl.create 10 in
        let rec visit_arc loops arc =
            match Hashtbl.find_opt visited arc.arc_dst with
            | None -> visit_node loops arc.arc_dst
            | Some `Visiting -> NodeSet.add arc.arc_dst loops
            | Some `Visited -> loops
        and visit_node loops node =
            (* Marking the current [node] as being visited currently, i.e. `Visiting *)
            Hashtbl.add visited node `Visiting;
            (* During this iteration and subsequent recursive calls,
               if we find this node, given its `Visiting marker, we will know
               we'll have found a loop. *)
            let ret = List.fold_left visit_arc loops node.node_out in
            (* Now marking it as `Visited, that is, 
               no longer needs to be considered for loops *)
            Hashtbl.add visited node `Visited;
            ret
        (* Iterate over each node at each entry point of every function in [cfg]
            and collect the loops found *)
        in List.fold_left visit_node NodeSet.empty (List.map (fun f -> f.func_entry) cfg.cfg_funcs)
    
    let iterate cfg =
        let _ = Random.self_init () in
        let widening_nodes = find_loop_nodes cfg in
        let rec iter_func dom f: Dom.t =
            (* This is the core of this algorithm *)
            dom
        and iter_arc dom arc: Dom.t =
            match arc.arc_inst with
            | CFG_skip(_) -> dom
            | CFG_assign (var, iexpr) -> Dom.assign dom var iexpr
            | CFG_guard (bexpr) -> Dom.guard dom bexpr
            | CFG_assert ((bexpr, _)) -> Dom.guard dom bexpr
            | CFG_call f -> iter_func dom f
        in
        let iter_node node : unit = Format.printf "<%i>: ⊤@ " node.node_id in
        List.fold_left iter_arc Dom.init cfg.cfg_arcs |> ignore;
        Format.printf "Node Values:@   @[<v 0>" ;
        List.iter iter_node cfg.cfg_nodes ;
        Format.printf "@]"
end

let iterate cfg =
  let _ = Random.self_init () in
  let iter_arc arc : unit = match arc.arc_inst with _ -> failwith "TODO" in
  let iter_node node : unit = Format.printf "<%i>: ⊤@ " node.node_id in
  List.iter iter_arc cfg.cfg_arcs ;
  Format.printf "Node Values:@   @[<v 0>" ;
  List.iter iter_node cfg.cfg_nodes ;
  Format.printf "@]"
