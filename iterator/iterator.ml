(*
  Cours "Sémantique et Application à la Vérification de programmes"

  Ecole normale supérieure, Paris, France / CNRS / INRIA
*)

open Frontend
open ControlFlowGraph
open Domains

module Make (Dom : Domain.DOMAIN) =
    struct
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
        let rec iter_func (f: func) : Dom.t =
            (* Use the fixpoint algorithm to get a mapping node => Dom.t for
               each analyzed node, starting at the function [f]'s entrypoint *)
            worklist_fix 
                (* We initialize the worklist with every successor of [node]. *)
                (NodeSet.of_list (List.map (fun arc -> arc.arc_dst) f.func_entry.node_out))
                (Dom.init) 
            
        and iter_arc (dom: Dom.t) (arc: arc) : Dom.t =
            match arc.arc_inst with
            | CFG_skip(_) -> dom
            | CFG_assign (var, iexpr) -> Dom.assign dom var iexpr
            | CFG_guard (bexpr) -> Dom.guard dom bexpr
            | CFG_assert ((bexpr, ext)) -> 
                    let domain_assertion_success = Dom.guard dom bexpr in
                    let domain_assertion_failure = Dom.guard dom (CFG_bool_unary(AST_NOT, bexpr)) in
                    Format.printf "---------------------------\n";
                    Format.printf "dom : %a\nsuccess : %a\nfailure : %a\n" Dom.pp dom Dom.pp dom Dom.pp dom;
                    Format.printf "---------------------------\n";
                    if not (Dom.is_bottom domain_assertion_failure) then
                            Format.printf "%a: %s \"%a\"@." ControlFlowGraphPrinter.pp_pos (fst ext) "Assertion failure" ControlFlowGraphPrinter.print_bool_expr bexpr;
                    domain_assertion_success
            | CFG_call f -> iter_func f
        and worklist_fix (wl: NodeSet.t) (env: Dom.t) : Dom.t =
            match NodeSet.choose_opt wl with
            | None -> env
            | Some node ->
                (* Get [node]'s associated domain, if not found, assume it is
                   [Dom.bottom] *)
                let iterable_arcs = node.node_in in
                let domains = List.map (iter_arc env) iterable_arcs in
                let new_domain = List.fold_left Dom.join Dom.bottom domains in
                Format.printf "--- worklist ---\ndom: %a\nnew_dom: %a\n--- end worklist ---\n" Dom.pp env Dom.pp new_domain;
                let new_env = 
                        (* In this case, the node's domain has been updated,
                            and we need to update it possibly using a widening *)
                        if NodeSet.mem node widening_nodes 
                            then Dom.widen env new_domain
                            else new_domain 
                in
                worklist_fix (List.fold_left (fun acc x -> NodeSet.add x.arc_dst acc) (NodeSet.remove node wl) node.node_out) new_env
        in
        match List.find_opt (fun f -> f.func_name = "main") cfg.cfg_funcs with
        | None -> ()
        | Some main -> 
            let _ = iter_func main in
            ()
end
