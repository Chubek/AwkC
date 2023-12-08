type 'a dag_node = {
  value: 'a;
  mutable children: 'a dag_node list;
}

type 'a dag = 'a dag_node list

let make_node value = { value; children = [] }

let add_edge node1 node2 =
  node1.children <- node2 :: node1.children

let topological_sort dag =
  let visited = Hashtbl.create 10 in
  let rec visit node acc =
    if Hashtbl.mem visited node then acc
    else begin
      Hashtbl.add visited node true;
      List.fold_left visit (node.value :: acc) node.children
    end
  in
  List.rev (List.fold_left visit [] dag)

let bfs graph start =
  let visited = Hashtbl.create 10 in
  let queue = Queue.create () in

  Queue.push start queue;
  Hashtbl.add visited start true;

  while not (Queue.is_empty queue) do
    let current = Queue.pop queue;

    try
      let neighbors = List.assoc current graph in
      List.iter (fun neighbor ->
        if not (Hashtbl.mem visited neighbor) then begin
          Queue.push neighbor queue;
          Hashtbl.add visited neighbor true;
        end
      ) neighbors
    with Not_found -> ()
  done;
