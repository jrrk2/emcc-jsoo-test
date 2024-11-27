(* main.ml *)

external my_function : int -> int = "_myFunction"

let () =
  (* Call the JavaScript function *)
  let result = my_function 5 in
  Printf.printf "Result from JS: %d\n" result
