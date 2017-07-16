let a1 = ("q0", [("q0", 'a', "q1"); ("q1",'b',"q1");("q1",'c',"q2")],["q2"]);;
let a2 = ("q0", [("q0", 'a', "q1"); ("q1",'b',"q1"); ("q1",'b',"q2");("q1",'c',"q2")],["q2"]);;

let get_transition_function (start,f,fs) = f;;

let rec rem_dups xs =
    match xs with
        | [] -> []
        | (x::xs') -> if List.mem x xs'
        then rem_dups xs'
        else x::rem_dups xs';;

let rec included_in xs ys =
    match xs with
        | [] -> true
        | (x::xs') -> List.mem x ys && included_in xs' ys;;

let rec domain f =
    match f with
    | [] -> []
    |((q1,s,q2)::xs) ->  q1::domain xs;;

let rec range f =
    match f with
    | [] -> []
    |((q1,s,q2)::xs) ->  q2::range xs;;

let states f =
    rem_dups((domain f) @ (range f));;
    
let next f q s =
    match f with
    | [] -> []
    | ((q1,s',q2)::xs) ->
        if (q1=q && s'=s)
        then q2::next xs q s
        else next xs q s;;

let is_det_a (start,f,fs) =
    let rec is_det_b =
        match f with
        | [] -> true
        | ((q1,s,q2)::xs) -> (List.length(next f q1 s) <= 1) && is_det_b xs
            in is_det_b f;;

let valid (start,f,fs) =
    let st = states f
    in List.mem start st && included_in fs st;;

let apply f (q:string) (s:char):string =
    match f with
    | [] -> failwith "Transition function cannot be applied"
    | ((q1,,s,q2)::xs) -> 
        if (q1 = q && s1 = s)
        then q2
        else apply xs q s;;

let accept (start,f,fs,) (w:char list) =
    let rec trace q w in
    match w with
    | [] List.mem q fs
    | (c:cs) -> trace (apply f q c) cs
in trace start w;;