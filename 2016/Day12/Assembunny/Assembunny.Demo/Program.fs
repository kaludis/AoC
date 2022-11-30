﻿open Assembunny

module ``Advent of Code 2016 Day 12`` =

    let demo cpu =
        let listing = 
            [ "cpy 1 a"
            ; "cpy 1 b"
            ; "cpy 26 d"
            ; "jnz c 2"
            ; "jnz 1 5"
            ; "cpy 7 c"
            ; "inc d"
            ; "dec c"
            ; "jnz c -2"
            ; "cpy a c"
            ; "inc a"
            ; "dec b"
            ; "jnz b -2"
            ; "cpy c b"
            ; "dec d"
            ; "jnz d -6"
            ; "cpy 14 c"
            ; "cpy 14 d"
            ; "inc a"
            ; "dec d"
            ; "jnz d -2"
            ; "dec c"
            ; "jnz c -5" ]

        match Executor.start cpu listing with
        | Ok cpu -> 
            cpu
            |> CPU.readRegisterA
            |> printfn "Register A contains: %d"
        | Error errors -> 
            printfn "Errors:"
            errors
            |> List.iter (printfn "\t%A")

[<EntryPoint>]
let main _ =
    CPU.init
    |> ``Advent of Code 2016 Day 12``.demo 

    CPU.init
    |> CPU.mapRegisterC (fun _ -> 1)
    |> ``Advent of Code 2016 Day 12``.demo
    
    0