namespace Assembunny.Tests

module ProgramExecutionTests =

    open NUnit.Framework
    open Assembunny

    [<SetUp>]
    let Setup () =
        ()

    [<Test>]
    let TestCPYProgram () =
        let listing =
            [ "cpy 10 a" 
            ; "cpy 11 b"
            ; "cpy 12 c"
            ; "cpy 13 d"]

        let expected =
            CPU.init
            |> CPU.mapRegisterA (fun _ -> 10)
            |> CPU.mapRegisterB (fun _ -> 11)
            |> CPU.mapRegisterC (fun _ -> 12)
            |> CPU.mapRegisterD (fun _ -> 13)
            |> CPU.mapRegisterIP (fun _ -> 4)

        match Executor.start CPU.init listing with
        | Error _ -> Assert.Fail()
        | Ok actual ->
            Assert.That(actual, Is.EqualTo expected)

    [<Test>]
    let TestIncProgram () =
        let listing =
            [ "inc a"
            ; "inc b"
            ; "inc c"
            ; "inc d"

            ; "inc b"
            ; "inc c"
            ; "inc d"

            ; "inc c"
            ; "inc d"

            ; "inc d" ]
        
        let expected =
            CPU.init
            |> CPU.mapRegisterA (fun _ -> 1)
            |> CPU.mapRegisterB (fun _ -> 2)
            |> CPU.mapRegisterC (fun _ -> 3)
            |> CPU.mapRegisterD (fun _ -> 4)
            |> CPU.mapRegisterIP (fun _ -> 10)
        
        match Executor.start CPU.init listing with
        | Error _ -> Assert.Fail()
        | Ok actual ->
            Assert.That(actual, Is.EqualTo expected)

    [<Test>]
    let TestDECProgram () =
        let listing =
            [ "dec a"
            ; "dec b"
            ; "dec c"
            ; "dec d"

            ; "dec b"
            ; "dec c"
            ; "dec d"

            ; "dec c"
            ; "dec d"

            ; "dec d" ]
        
        let expected =
            CPU.init
            |> CPU.mapRegisterA (fun _ -> 0)
            |> CPU.mapRegisterB (fun _ -> 0)
            |> CPU.mapRegisterC (fun _ -> 0)
            |> CPU.mapRegisterD (fun _ -> 0)
            |> CPU.mapRegisterIP (fun _ -> 10)
        
        let cpu = 
            CPU.init
            |> CPU.mapRegisterA (fun _ -> 1)
            |> CPU.mapRegisterB (fun _ -> 2)
            |> CPU.mapRegisterC (fun _ -> 3)
            |> CPU.mapRegisterD (fun _ -> 4)

        match Executor.start cpu listing with
        | Error _ -> Assert.Fail()
        | Ok actual ->
            Assert.That(actual, Is.EqualTo expected)

    [<Test>]
    let TestCrossProgram () =
        let listing =
            [ "cpy 4 d"
            ; "cpy d a"
            ; "dec a"
            ; "inc b"
            ; "jnz a -2"
            ; "jnz b 5" ]

        let expected =
            CPU.init
            |> CPU.mapRegisterA (fun _ -> 0)
            |> CPU.mapRegisterB (fun _ -> 4)
            |> CPU.mapRegisterD (fun _ -> 4)
            |> CPU.mapRegisterIP (fun _ -> 10)

        match Executor.start CPU.init listing with
        | Error _ -> Assert.Fail()
        | Ok actual ->
            Assert.That(actual, Is.EqualTo expected)        

