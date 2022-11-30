namespace Assembunny.Tests

module ProgramParserTests =

    open NUnit.Framework
    open Assembunny
    open Assembunny.ProgramParser

    [<SetUp>]
    let Setup () =
        ()

    [<Test>]
    let TestEmptyProgramParse () =
        let program = [""]
        let expected = [ParseError.BadInstruction "Mailformed instruction ''"]
        match parseProgram program with
        | Ok _ -> Assert.Fail()
        | Error errors ->
            Assert.That(errors, Is.EqualTo expected)

    [<Test>]
    let TestBadInstructions () =
        let program = 
            [ "aaa 10 a" 
            ; "xx 20"]

        let expected = 
            [ ParseError.BadInstruction "Mailformed instruction 'aaa 10 a'"
            ; ParseError.BadInstruction "Mailformed instruction 'xx 20'" ]

        match parseProgram program with
        | Ok _ -> Assert.Fail()
        | Error errors ->
            Assert.That(errors, Is.EqualTo expected)

    [<Test>]
    let TestBadRegOperands () =
        let program = 
            [ "cpy 41 x" 
            ; "inc f"]

        let expected = 
            [ ParseError.BadRegisterOperand "Bad register operand name 'x'"
            ; ParseError.BadRegisterOperand "Bad register operand name 'f'" ]

        match parseProgram program with
        | Ok _ -> Assert.Fail()
        | Error errors ->
            Assert.That(errors, Is.EqualTo expected)

    [<Test>]
    let TestBadSrcOperand () =
        let program = 
            [ "cpy q a" ]

        let expected = 
            [ ParseError.BadSrcOperand "Bad src operand 'q'" ]

        match parseProgram program with
        | Ok _ -> Assert.Fail()
        | Error errors ->
            Assert.That(errors, Is.EqualTo expected)


    [<Test>]
    let TestBadIntOperand () =
        let program = 
            [ "jnz b t"]

        let expected = 
            [ ParseError.BadValueOperand "Bad integer operand value 't'" ]

        match parseProgram program with
        | Ok _ -> Assert.Fail()
        | Error errors ->
            Assert.That(errors, Is.EqualTo expected)

    [<Test>]
    let TestNormalRegSrcOperand () =
        let program = 
            [ "cpy b a" ]

        let expected = 
            [ Instruction.CPY (Src.Register RegisterName.B, RegisterName.A ) ]
            |> Program

        match parseProgram program with
        | Ok actual -> Assert.That(actual, Is.EqualTo expected)
        | Error errors -> Assert.Fail()


    [<Test>]
    let TestNormalScalarSrcOperand () =
        let program = 
            [ "cpy 10 a" ]

        let expected = 
            [ Instruction.CPY (Src.Scalar 10, RegisterName.A ) ]
            |> Program

        match parseProgram program with
        | Ok actual -> Assert.That(actual, Is.EqualTo expected)
        | Error errors -> Assert.Fail()

    [<Test>]
    let TestNormalProgram () =
        let program =
            [ "cpy 41 a" 
            ; "inc a"
            ; "dec b"
            ; "cpy a c"
            ; "jnz c 3"]

        let expected =
            [ Instruction.CPY (Src.Scalar 41, RegisterName.A)
            ; Instruction.INC RegisterName.A
            ; Instruction.DEC RegisterName.B
            ; Instruction.CPY (Src.Register RegisterName.A, RegisterName.C)
            ; Instruction.JNZ (Src.Register RegisterName.C, 3) ]
            |> Program

        match parseProgram program with
        | Ok p ->
            Assert.That(p, Is.EqualTo expected)
        | Error _ -> Assert.Fail()