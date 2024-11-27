open Js_of_ocaml
open Js_of_ocaml_tyxml

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

(* Existing dialog creation functions remain the same *)

let set_static_text element txt =
  element##.textContent := Js.some (Js.string txt);
  Js.Unsafe.set (element##.style) (Js.string "display") (Js.string "block")
    
let confirm_my_button = fun _ ->
  let element = Js_of_ocaml.Dom_html.getElementById "extra-message" in
  let quotient = _myFloat 355. 113. in
  set_static_text element ("Button was clicked: "^Printf.sprintf "Result from JS: %10.6f\n" quotient);

(*

let () =
  (* Call the JavaScript functions *)
  let result = _myFunction 5 in
  Printf.printf "Result from JS: %d\n" result;
  let ascii = string_to_float "Solaris" in
  Printf.printf "Converted ASCII = %10.2f\n" ascii;
  Printf.printf "Re-converted ASCII = %s\n" (float_to_string ascii);  
  *)
true

let create_extra_picker () =
  let open Tyxml_js.Html in
  let open Js_of_ocaml in
  let message_div = div ~a:[a_id "extra-message"; a_style "padding-top: 15px; font-size: 16px; color: #333; display: none;"] [txt ""] in
  let select_div = div ~a:[a_id "extra-select"; a_style "padding-top: 15px; font-size: 16px; color: #333; display: none;"] [txt ""] in
  let button = button ~a:[ a_id "my-button"; a_onclick confirm_my_button ] [ txt "Click Me" ] in
  let input = input ~a:[ a_id "my-input"; a_placeholder "Type here" ; a_oninput (fun _ -> true) ] () in
  let dropdown =
    select
      ~a:[ 
        a_id "my-dropdown";
        a_onchange (fun ev ->
          Js.Opt.case (ev##.target)
            (fun () -> false)
            (fun target ->
              let select = Dom_html.CoerceTo.select target in
              Js.Opt.case select
                (fun () -> false)
                (fun select ->
                  let selected_value = Js.to_string (select##.value) in
		  let element = Js_of_ocaml.Dom_html.getElementById "extra-select" in
		  send selected_value;
		  set_static_text element ("Planet was selected: "^selected_value);
                  true
                )
            )
        )
      ]
      (List.map
         (fun opt -> option ~a:[ a_value opt ] (txt opt))
         ["Mercury";"Venus";"Earth";"Mars";"Jupiter";"Saturn";"Uranus";"Neptune"])
  in
  let output = div ~a:[ a_id "output"; a_style "margin-top: 20px;" ] [] in
  div [ button; br (); input; br (); dropdown; br (); output; message_div; select_div ]
      
let create_date_picker () =
  let open Tyxml_js.Html in
  let label = label ~a:[a_label_for "date-picker"] [txt "Pick a date: "] in
  let input = input ~a:[a_id "date-picker"; a_input_type `Date] () in
  div [label; br (); input]

let tz_local () =
    let dummy = (Js.Unsafe.obj [||]) in
    let intl = Js.Unsafe.global##.Intl in
    let date = intl##DateTimeFormat(dummy) in
    let options = date##resolvedOptions(dummy) in
    let tz = options##.timeZone in
    Js.to_string tz

let create_color_picker () =
  let open Tyxml_js.Html in
  let label = label ~a:[a_label_for "color-picker"] [txt "Pick a color: "] in
  let input = input ~a:[a_id "color-picker"; a_input_type `Color] () in
  div [label; br (); input]

(* Previous helper functions remain the same *)

(* Tab configuration type *)
type tab_config = {
  id: string;
  label: string;
  description: string;  (* New field for hover description *)
  content: Html_types.div Tyxml_js.Html.elt;
}

let switch_tab tab_id =
  let headers = Dom_html.document##getElementsByClassName (Js.string "3d-card-index-tab") in
  let contents = Dom_html.document##getElementsByClassName (Js.string "3d-card-index-content") in
  
  for i = 0 to headers##.length - 1 do
    let header = headers##item i in
    let content = contents##item i in
    match Js.Opt.to_option header, Js.Opt.to_option content with
    | Some h, Some c ->
        if Js.to_string h##.id = (tab_id ^ "-tab") then begin
          h##.style##.transform := Js.string "rotateX(-5deg)";
          Js.Unsafe.set (h##.style) (Js.string "boxShadow") (Js.string "0 10px 20px rgba(0,0,0,0.19), 0 6px 6px rgba(0,0,0,0.23)");
          h##.style##.backgroundColor := Js.string "white";
          h##.style##.zIndex := Js.string "10";
          c##.style##.display := Js.string "block"
        end else begin
          h##.style##.transform := Js.string "rotateX(0deg) translateY(15px)";
          Js.Unsafe.set (h##.style) (Js.string "boxShadow") (Js.string "0 1px 3px rgba(0,0,0,0.12), 0 1px 2px rgba(0,0,0,0.24)");
          h##.style##.backgroundColor := Js.string "#f0f0f0";
          h##.style##.zIndex := Js.string "1";
          c##.style##.display := Js.string "none"
        end
    | _ -> ()
  done;
  true

let create_tab_headers tabs =
  let open Tyxml_js.Html in
  let base_style = 
    "padding: 10px 15px; margin-right: 5px; " ^
    "border: 1px solid #ddd; " ^
    "border-bottom: none; " ^
    "border-radius: 8px 8px 0 0; " ^
    "transition: all 0.3s cubic-bezier(.25,.8,.25,1); " ^
    "position: relative;"
  in
  div ~a:[
    a_class ["3d-card-index-headers"]; 
    a_style "display: flex; perspective: 1000px; margin-bottom: -1px;"
  ] (
    List.mapi (fun index tab ->
      let is_first = (index = 0) in
      button ~a:[
        a_class ["3d-card-index-tab"; if is_first then "active" else ""];
        a_id (tab.id ^ "-tab");
        a_onclick (fun _ -> switch_tab tab.id);
        a_style (base_style ^ 
                 (if is_first then 
                   "transform: rotateX(-5deg);" ^
                   "box-shadow: 0 10px 20px rgba(0,0,0,0.19), 0 6px 6px rgba(0,0,0,0.23);" ^
                   "background-color: white;"
                 else 
                   "transform: rotateX(0deg) translateY(" ^ string_of_int ((index + 1) * 5) ^ "px);" ^
                   "box-shadow: 0 1px 3px rgba(0,0,0,0.12), 0 1px 2px rgba(0,0,0,0.24);" ^
                   "background-color: #f0f0f0;") ^
                 (* Tooltip styles embedded in the button's style *)
                 "position: relative;" ^
                 "group");
        a_onmouseover (fun _ ->
          let tooltip = Js_of_ocaml.Dom_html.getElementById (tab.id ^ "-tooltip") in
          tooltip##.style##.display := Js.string "block";
          true
        );
        a_onmouseout (fun _ ->
          let tooltip = Js_of_ocaml.Dom_html.getElementById (tab.id ^ "-tooltip") in
          tooltip##.style##.display := Js.string "none";
          true
        )
      ] [txt tab.label];
    ) tabs @
    (* Create separate tooltip divs *)
    List.mapi (fun index tab ->
      div ~a:[
        a_id (tab.id ^ "-tooltip");
        a_style ("display: none; " ^
                 "position: absolute; " ^
                 "bottom: 100%; " ^
                 "left: 50%; " ^
                 "transform: translateX(-50%); " ^
                 "background-color: #333; " ^
                 "color: white; " ^
                 "padding: 5px 10px; " ^
                 "border-radius: 4px; " ^
                 "z-index: 10; " ^
                 "white-space: nowrap;")
      ] [txt tab.description]
    ) tabs
  )

let create_tab_container () =
  let open Tyxml_js.Html in
  let tabs = [
    { 
      id = "date"; 
      label = "Date"; 
      description = "Select and pick a specific date";
      content = create_date_picker (); 
      };
    { 
      id = "extra"; 
      label = "Extra"; 
      description = "Additional interactive elements";
      content = create_extra_picker (); 
    };
    { 
      id = "color"; 
      label = "Color"; 
      description = "Pick a color from the palette";
      content = create_color_picker (); 
    }
  ] in
  
  let tab_headers = create_tab_headers tabs in
  let tab_contents = 
    div ~a:[
      a_class ["3d-card-index-contents"]; 
      a_style ("border: 2px solid #ddd; " ^
              "padding: 20px; " ^
              "background-color: white;" ^
              "box-shadow: 0 10px 20px rgba(0,0,0,0.19), 0 6px 6px rgba(0,0,0,0.23);" ^
              "transform: translateY(-10px);")
    ] (
      List.mapi (fun index tab ->
        div ~a:[
          a_class ["3d-card-index-content"; if index = 0 then "active" else ""];
          a_id (tab.id ^ "-content");
          a_style (if index = 0 then "" else "display: none;")
        ] [tab.content]
      ) tabs
    )
  in
  div [tab_headers; tab_contents]

(* Rest of the module remains the same as in previous implementation *)

let create_ui () =
  let open Tyxml_js.Html in
  div ~a:[a_style "max-width: 600px; margin: 0 auto; padding: 20px;"] [
    create_tab_container ();
    br ();
    div ~a:[a_id "output"; a_style "margin-top: 20px; padding: 10px; background-color: #f9f9f9;"] []
  ]

let () =
  let root = Dom_html.getElementById "app" in
  Dom.appendChild root (Tyxml_js.To_dom.of_div (create_ui ()))
