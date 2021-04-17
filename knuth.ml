
module Knuth =
  struct
      (**@author Hugo Bernedo*)

    (*
    Utiliser kmp & bm 
    Attention la premier String est l'occurrences à tester & la seconde est la chaîne de caractères.
     *)
    
    (*
    let tous_to_strlst a =
      let rec ttsrec b acc =
        match b with
        | [] -> acc
        | h :: t -> ttsrec t (acc ^ (string_of_code h))
      in ttsrec a "";;*)


    
    (**Comparaison chaine de caractere entre deux positions
     *)
    let compare_substrings w k w2 k2 s =
      let rec aux i =
        if i = s then true
        else w.[k + i] = w2.[k2 + i] && aux (i + 1)
      in
      aux 0
    ;;


    (**Verifie le nbre d'occurences
     *)
    let occurences x t =
      let m = String.length x
      and n = String.length t in
      let result = ref [] in
      for k = 0 to n - m do
        if compare_substrings t k x 0 m
        then result := k :: !result
      done;
      !result
    ;;


    let border_length w =
      let s = String.length w in
      let rec aux i =
        if i = 0 || compare_substrings w 0 w (s - i ) i
        then i else aux (i-1)
      in
      aux (s-1)
    ;;

    
    let borders x =
      let m = String.length x in
      let beta = Array.make (m+1) (-1) in
      let rec aux j a  =
        if j = 0 then  0
        else if x.[beta.(j)] = a then beta.(j) +1
        else aux beta.(j) a
      in
      for j = 1 to m do
        beta.(j) <- aux (j-1) x.[j-1]
      done;
      beta
    ;;

    (**Programme de recherche Knuth-Morris-Pratt
     *)
    let kmp x t =
      let beta = borders x in
      let m = String.length x in
      let n = String.length t in
      let k = ref 0 in
      let j = ref 0 in
      let result = ref [] in
      while !k <= n -m do
        while !j < m && x.[!j] = t.[!k + !j] do j:= !j +1 done;
        if !j = m then result := !k :: !result ;
        k:= !k + !j - beta.(!j);
        j := max 0 beta.(!j);
      done;
      !result
    ;;

    let reskmp x t = if (kmp x t) = [] then "Code mauvais" else "Bien joué";;


    
    let first_occurence x =
      let o = Array.make 256 0 in
      for j = String.length x - 1 downto 0 do
        o.(int_of_char x.[j] ) < - j
      done;
      o
    ;;


    let good_suffix x =
      let m = String.length x in
      let beta = borders x in
      let gamma = Array.make (m+1) (m- beta.(m)) in
      for i = m downto 1 do
        if gamma.(beta.(i)) > i - beta.(i) then true else false
      done;
      gamma
    ;;


    let match_length x t k =
      let m = String.length x in
      let rec aux j =
        if j = m or x.[j] <> t.[k+j]
        then j
        else aux (j+1) in
      aux 0
    ;;


    (** Programme de recherche Boyer-Moore
     *)
    let bm x t =
      let n = String.length t in
      let m = String.length x in
      let first = first_occurence x in
      let gamma = good_suffix x in
      let k = ref (n-m) in
      let result = ref [] in
      while !k >= 0 do
        let j = match_length x t !k in
        if j = m then begin
            result := !k :: !result;
            k:= !k - gamma.(m)
          end
        else k:= !k - (max gamma.(j) (first.(int_of_char t.[!k+j]) - j ))
      done;
      !result
    ;;
    
  end;;
open Knuth;;
