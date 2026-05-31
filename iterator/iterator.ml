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
            let iter_arc (dom: Dom.t) (arc: arc) : Dom.t =
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
                in
            let rec loop (states: Dom.t NodeMap.t) (wl: NodeSet.t) : Dom.t NodeMap.t =
                match NodeSet.choose_opt wl with
                | None -> states
                | Some node ->
                        let wl = NodeSet.remove node wl in
                        let in_state = 
                            match NodeMap.find_opt node states with
                            | Some dom -> dom
                            | None -> Dom.bottom
                        in
                        let states, wl =
                            List.fold_left
                                (fun (states, wl) arc ->
                                    let out_state = iter_arc in_state arc in
                                    let dst_old =
                                        match NodeMap.find_opt arc.arc_dst states with
                                        | Some dom -> dom
                                        | None -> Dom.bottom
                                    in
                                    let dst_join = Dom.join dst_old out_state in
                                    let dst_new =
                                        if NodeSet.mem arc.arc_dst widening_nodes
                                        then Dom.widen dst_old dst_join
                                        else dst_join
                                    in
                                    if Dom.leq dst_new dst_old then (states, wl)
                                    else (NodeMap.add arc.arc_dst dst_new states, NodeSet.add arc.arc_dst wl))
                                (states, wl)
                                node.node_out
                        in
                        loop states wl
                in
                let states0 = NodeMap.add f.func_entry Dom.init NodeMap.empty in
                let final_states = loop states0 (NodeSet.singleton f.func_entry) in
                match NodeMap.find_opt f.func_exit final_states with
                | Some dom -> dom
                | None -> Dom.bottom
            in
            match List.find_opt (fun f -> f.func_name = "main") cfg.cfg_funcs with
            | None -> ()
            | Some main -> 
                let _ = iter_func main in
                ()
end
