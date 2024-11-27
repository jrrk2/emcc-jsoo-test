(* main.ml *)

external _myFunction : int -> int = "_myFunction"
external _myFloat : float -> float -> float = "_myFloat"
external _myAscii : float -> unit = "_myAscii"

let string_to_float (s : string) : float =
  let len = String.length s in
  let rec aux acc i =
    if i < len then
      aux (acc *. 128.0 +. float_of_int (127 land (Char.code s.[i]))) (i + 1)
    else
      acc
  in
aux 0.0 0

let rec float_to_string f =
  let flr = floor (f /. 128.0) in let f' = f -. flr *. 128.0 in
  (if flr > 0.0 then float_to_string flr else "") ^ String.make 1 (Char.chr (int_of_float f'))

let send str = _myAscii (string_to_float str)

let () =
  (* Call the JavaScript functions *)
  let result = _myFunction 5 in
  Printf.printf "Result from JS: %d\n" result;
  let quotient = _myFloat 355. 113. in
  Printf.printf "Result from JS: %10.5f\n" quotient; 
  let ascii = string_to_float "Solaris" in
  Printf.printf "Converted ASCII = %10.2f\n" ascii;
  Printf.printf "Re-converted ASCII = %s\n" (float_to_string ascii);
  
  List.iter send ["Mercury";"Venus";"Earth";"Mars";"Jupiter";"Saturn";"Uranus";"Neptune"]


