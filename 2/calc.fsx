let lines =
    System.IO.File.ReadAllLines("casino.txt")
    |> Array.map(fun x -> x.Split([|'\t'|]))

lines
|> Array.sumBy(fun x -> 
    x
    |> Array.sumBy(fun y -> if y = "W" then 1 else 0)
    )
|> (fun x -> printfn "n = %i" x)

lines
|> Array.sumBy(fun x -> 
    x
    |> Array.sumBy(fun y -> if y = "L" then 1 else 0)
    )
|> (fun x -> printfn "m = %i" x)

lines
|> Array.sumBy(fun x -> if x.[x.Length - 1] = "W" then 1 else 0)
|> (fun x -> printfn "k = %i" x)

lines
|> Array.sumBy(fun x -> 
    if x.Length < 1 then 0 else
    x.[0..x.Length - 2]
    |> Array.sumBy(fun y -> if y = "W" then 1 else 0)
    )
|> (fun x -> printfn "l = %i" x)

lines
|> Array.sumBy(fun x -> if x.[x.Length - 1] = "L" then 1 else 0)
|> (fun x -> printfn "u = %i" x)



lines
|> Array.sumBy(fun x -> 
    if x.Length < 1 then 0 else
    x.[0..x.Length - 2]
    |> Array.sumBy(fun y -> if y = "L" then 1 else 0)
    )
|> (fun x -> printfn "f = %i" x)

lines
|> Array.sumBy(fun x -> x.Length - 1)
|> (fun x -> printfn "f = %i" x)