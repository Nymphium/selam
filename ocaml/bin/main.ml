let e = Selam.Syntax.(
    let*! x = atom 3 in
    let*! k =
      lama @@ fun y ->
      lama @@ fun _ -> !y
    in
    (!k @ !x) @ atom 5
  )

let () =
  print_endline @@ Selam.(Eval.eval e |> Abs.show_w)
