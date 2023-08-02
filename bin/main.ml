open Plt.Kast
open Plt.Trans

let _ =
  (* let lisp = Symbol "abc" in
     Format.printf "%a\n" pp_lisp lisp;
     let kexp = trans lisp (KCont { k = Sym "halt" }) in
     Format.printf "%a\n" pp_term kexp;

     let lisp = Terms [ Symbol "fn"; Terms [ Symbol "x" ]; Symbol "y" ] in
     Format.printf "%a\n" pp_lisp lisp;
     let kexp = trans lisp (KCont { k = Sym "halt" }) in
     Format.printf "%a\n" pp_term kexp; *)

  (* let lisp = Terms [ Symbol "f"; Symbol "x" ] in
     (* Format.printf "%a\n" pp_lisp lisp; *)
     let kexp = trans lisp (KCont { k = Sym "halt" }) in
     Format.printf "%a\n" pp_term kexp; *)
  (* let lisp =
       Terms
         [ Terms [ Symbol "fn"; Terms [ Symbol "x" ]; Symbol "x" ]; Symbol "x" ]
     in *)
  (* let lisp = Terms [ Symbol "fn"; Terms [Symbol "x"]; Terms [Symbol "+"; Symbol "x"; Symbol "x"] ] in *)
  (* let lisp =
       Terms
         [
           Terms
             [
               Symbol "fn";
               Terms [ Symbol "x"; Symbol "y" ];
               Terms [ Symbol "#+"; Terms [Symbol "cons"; Symbol "x"; Symbol "y"] ];
             ];
           Lit "1";
           Lit "2";
         ]
     in *)
  (* let lisp =
       Terms
         [
           Symbol "#+";
           Terms
             [
               Symbol "cons";
               Lit "2";
               Terms
                 [
                   Symbol "call/cc";
                   Terms
                     [
                       Symbol "fn";
                       Terms [ Symbol "f" ];
                       Lit "5"
                       (* Terms [ Symbol "f"; Lit "1" ]; *)
                     ];
                 ];
             ];
         ]
     in *)
  (* let lisp =
       Terms
         [
           Symbol "#+";
           Terms
             [
               Symbol "cons";
               Lit "2";
               Terms
                 [
                   Symbol "call/cc";
                   Terms
                     [
                       Symbol "fn";
                       Terms [ Symbol "f" ];
                       Lit "5"
                       (* Terms [ Symbol "f"; Lit "1" ]; *)
                     ];
                 ];
             ];
         ]
     in *)
  let lisp =
    Terms
      [
        Terms
          [
            Symbol "fn"; Terms [ Symbol "f" ]; Terms [ Symbol "f"; Symbol "f" ];
          ];
        Terms
          [
            Symbol "call/cc";
            Terms [ Symbol "fn"; Terms [ Symbol "x" ]; Symbol "x" ];
          ];
      ]
  in
  (* let lisp = Terms [Symbol "if"; Symbol "false"; Lit "1"; Lit "2"] in *)
  (* let lisp = Terms [Symbol "#+"; Lit "1"; Lit "2"] in *)
  (* let lisp = Terms [Symbol "-"; Lit "1"] in *)
  (* let lisp = Terms [ Symbol "#+"; Terms [Symbol "cons"; Lit "1"; Lit "2"] ] in *)
  (* let lisp = Terms [ Symbol "if"; Symbol "nil"; Symbol "x"; Symbol "y"] in *)
  (* let lisp =
       Terms
         [
           Terms
             [
               Symbol "fn";
               Terms [ Symbol "x" ];
               Terms [ Symbol "+"; Symbol "x"; Symbol "x" ];
             ];
           Symbol "y";
         ]
     in *)
  (* let lisp = Terms [Symbol "+"; Symbol "x"; Symbol "x"] in *)
  (* let lisp = Symbol "x" in *)
  (* Format.printf "%a\n" pp_lisp lisp; *)
  let kexp = trans lisp (KCont { k = Sym "halt" }) in
  Format.printf "%a\n" Plt.Kast.pp kexp;
  let value = Plt.Eval.eval kexp in
  Format.printf "%a\n" Plt.Eval.Value.pp value;
  true
