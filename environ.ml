type symbols_table_node = {
  name: identifier;
  tyy: symbol_type;
  tree: program;
  next: symbols_table option;
}

type symbols_table = symbols_table_node option

let symtab_insert ~sym_table name tyy tree : symbols_table = 
        match sym_table with
      | None -> {name;tyy;tree;next=None;}
      | Some -> 
                      let {nm,ty,tr,next} = sym_table in
                      match next with
                    | None -> {nm;ty;tr;Some {name;tyy;tree;None}}
                    | Some -> symtab_insert ~sym_table:sym_table name tyy tree
;;
        
