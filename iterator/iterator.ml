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
        let rec iter_func (f: func) : Dom.t =
            (* Use the fixpoint algorithm to get a mapping node => Dom.t for
               each analyzed node, starting at the function [f]'s entrypoint *)
            let env = worklist_fix 
                (* We initialize the worklist with every successor of [node]. *)
                (NodeSet.of_list (List.map (fun arc -> arc.arc_dst) f.func_entry.node_out))
                (NodeMap.empty) 
            in
            (* Collect the calculated domain and return it *)
            match NodeMap.find_opt f.func_exit env with
            | None -> Dom.bottom
            | Some d -> d
        and iter_arc (dom: Dom.t) (arc: arc) : Dom.t =
            match arc.arc_inst with
            | CFG_skip(_) -> dom
            | CFG_assign (var, iexpr) -> Dom.assign dom var iexpr
            | CFG_guard (bexpr) -> Dom.guard dom bexpr
            | CFG_assert ((bexpr, _)) -> Dom.guard dom bexpr
            | CFG_call f -> iter_func f
        and worklist_fix (wl: NodeSet.t) (env: Dom.t NodeMap.t) : Dom.t NodeMap.t =
            match NodeSet.choose_opt wl with
            | None -> env
            | Some node ->
                (* Get [node]'s associated domain, if not found, assume it is
                   [Dom.bottom] *)
                let node_domain = NodeMap.find_opt node env |> Option.value ~default:Dom.bottom in
                let iterable_arcs = List.filter (fun arc -> NodeMap.mem arc.arc_src env) node.node_in in
                let domains = List.map (iter_arc node_domain) iterable_arcs in
                let new_domain = List.fold_left Dom.join Dom.bottom domains in
                let env = 
                    if node_domain <> new_domain then
                        (* In this case, the node's domain has been updated,
                            and we need to update it possibly using a widening *)
                        let new_domain = if NodeSet.mem node widening_nodes then Dom.widen node_domain new_domain
                        else new_domain in
                        NodeMap.add node new_domain env
                    else env
                in
                worklist_fix (List.fold_left (fun acc x -> NodeSet.add x.arc_dst acc) wl node.node_out) env
        in
        match List.find_opt (fun f -> f.func_name = "main") cfg.cfg_funcs with
        | None -> ()
        | Some main -> 
            let _ = iter_func main in
            ()
end

let iterate cfg =
  let _ = Random.self_init () in
  let iter_arc arc : unit = match arc.arc_inst with _ -> failwith "TODO" in
  let iter_node node : unit = Format.printf "<%i>: ⊤@ " node.node_id in
  List.iter iter_arc cfg.cfg_arcs ;
  Format.printf "Node Values:@   @[<v 0>" ;
  List.iter iter_node cfg.cfg_nodes ;
  Format.printf "@]"
