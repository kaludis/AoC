namespace Assembunny.Tests

module InstructionsTranslationTests =

    open NUnit.Framework
    open Assembunny

    [<SetUp>]
    let Setup () =
        ()

    [<Test>]
    let TestTranslateCPYScalarSrcInstruction () =
        let instruction = Instruction.CPY (Src.Scalar 1, RegisterName.A)

        let expected =
            CPU.init
            |> CPU.mapRegisterA (fun _ -> 1)
            |> CPU.mapRegisterIP (fun _ -> 1)

        let (CPUOp op) = OpTranslator.translateInstruction instruction

        let actual = op CPU.init

        Assert.That(actual, Is.EqualTo expected)

    [<Test>]
    let TestTranslateCPYRegSrcInstruction () =
        let instruction = Instruction.CPY (Src.Register RegisterName.B, RegisterName.A)

        let expected =
            CPU.init
            |> CPU.mapRegisterA (fun _ -> 2)
            |> CPU.mapRegisterB (fun _ -> 2)
            |> CPU.mapRegisterIP (fun _ -> 1)

        let (CPUOp op) = OpTranslator.translateInstruction instruction

        let actual = 
            CPU.init
            |> CPU.mapRegisterB (fun _ -> 2)
            |> op 

        Assert.That(actual, Is.EqualTo expected)

    [<Test>]
    let TestTranslateCPYScalarSrcInstructionSameReg () =
        let ops =
            [ Instruction.CPY (Src.Scalar 1, RegisterName.A)
            ; Instruction.CPY (Src.Scalar 1, RegisterName.A)
            ; Instruction.CPY (Src.Scalar 1, RegisterName.A)
            ; Instruction.CPY (Src.Scalar 1, RegisterName.A) ]
            |> List.map OpTranslator.translateInstruction

        let expected =
            CPU.init
            |> CPU.mapRegisterA (fun _ -> 1)
            |> CPU.mapRegisterIP (fun _ -> 4)

        let actual = List.fold (fun cpu (CPUOp op) -> op cpu) CPU.init ops

        Assert.That(actual, Is.EqualTo expected)

    [<Test>]
    let TestTranslateCPYRegSrcInstructionSameReg () =
        let ops =
            [ Instruction.CPY (Src.Register RegisterName.B , RegisterName.A)
            ; Instruction.CPY (Src.Register RegisterName.C , RegisterName.A)
            ; Instruction.CPY (Src.Register RegisterName.D , RegisterName.A) ]
            |> List.map OpTranslator.translateInstruction

        let expected =
            CPU.init
            |> CPU.mapRegisterA (fun _ -> 5)
            |> CPU.mapRegisterB (fun _ -> 1)
            |> CPU.mapRegisterC (fun _ -> 3)
            |> CPU.mapRegisterD (fun _ -> 5)
            |> CPU.mapRegisterIP (fun _ -> 3)

        let cpu =
            CPU.init
            |> CPU.mapRegisterB (fun _ -> 1)
            |> CPU.mapRegisterC (fun _ -> 3)
            |> CPU.mapRegisterD (fun _ -> 5)

        let actual = List.fold (fun cpu (CPUOp op) -> op cpu) cpu ops

        Assert.That(actual, Is.EqualTo expected)

    [<Test>]
    let TestTranslateCPYScalarSrcInstructionAllRegs () =
        let ops =
            [ Instruction.CPY (Src.Scalar 1, RegisterName.A)
            ; Instruction.CPY (Src.Scalar 2, RegisterName.B)
            ; Instruction.CPY (Src.Scalar 3, RegisterName.C)
            ; Instruction.CPY (Src.Scalar 4, RegisterName.D) ]
            |> List.map OpTranslator.translateInstruction

        let expected =
            CPU.init
            |> CPU.mapRegisterA (fun _ -> 1)
            |> CPU.mapRegisterB (fun _ -> 2)
            |> CPU.mapRegisterC (fun _ -> 3)
            |> CPU.mapRegisterD (fun _ -> 4)
            |> CPU.mapRegisterIP (fun _ -> 4)

        let actual = List.fold (fun cpu (CPUOp op) -> op cpu) CPU.init ops

        Assert.That(actual, Is.EqualTo expected)

    [<Test>]
    let TestTranslateCPYRegSrcInstructionAllRegs () =
        let ops =
            [ Instruction.CPY (Src.Register RegisterName.A, RegisterName.B)
            ; Instruction.CPY (Src.Register RegisterName.B, RegisterName.C)
            ; Instruction.CPY (Src.Register RegisterName.C, RegisterName.D) ]
            |> List.map OpTranslator.translateInstruction

        let expected =
            CPU.init
            |> CPU.mapRegisterA (fun _ -> 7)
            |> CPU.mapRegisterB (fun _ -> 7)
            |> CPU.mapRegisterC (fun _ -> 7)
            |> CPU.mapRegisterD (fun _ -> 7)
            |> CPU.mapRegisterIP (fun _ -> 3)

        let cpu =
            CPU.init
            |> CPU.mapRegisterA (fun _ -> 7)

        let actual = List.fold (fun cpu (CPUOp op) -> op cpu) cpu ops

        Assert.That(actual, Is.EqualTo expected)

    [<Test>]
    let TestTranslateINCInstruction () =
        let (CPUOp op) =
            Instruction.INC RegisterName.A
            |> OpTranslator.translateInstruction

        let expected =
            CPU.init
            |> CPU.mapRegisterA (fun _ -> 1)
            |> CPU.mapRegisterIP (fun _ -> 1)

        let actual = op CPU.init

        Assert.That(actual, Is.EqualTo expected)

    [<Test>]
    let TestTranslateINCInstructionSameReg () =
        let ops =
            [ Instruction.INC RegisterName.A
            ; Instruction.INC RegisterName.A
            ; Instruction.INC RegisterName.A ]
            |> List.map OpTranslator.translateInstruction

        let expected =
            CPU.init
            |> CPU.mapRegisterA (fun _ -> 3)
            |> CPU.mapRegisterIP (fun _ -> 3)

        let actual = List.fold (fun cpu (CPUOp op) -> op cpu) CPU.init ops

        Assert.That(actual, Is.EqualTo expected)


    [<Test>]
    let TestTranslateINCInstructionAllRegs () =
        let ops =
            [ Instruction.INC RegisterName.A
            ; Instruction.INC RegisterName.B
            ; Instruction.INC RegisterName.B
            ; Instruction.INC RegisterName.C
            ; Instruction.INC RegisterName.C
            ; Instruction.INC RegisterName.C
            ; Instruction.INC RegisterName.D
            ; Instruction.INC RegisterName.D
            ; Instruction.INC RegisterName.D
            ; Instruction.INC RegisterName.D ]
            |> List.map OpTranslator.translateInstruction

        let expected =
            CPU.init
            |> CPU.mapRegisterA (fun _ -> 1)
            |> CPU.mapRegisterB (fun _ -> 2)
            |> CPU.mapRegisterC (fun _ -> 3)
            |> CPU.mapRegisterD (fun _ -> 4)
            |> CPU.mapRegisterIP (fun _ -> 10)

        let actual = List.fold (fun cpu (CPUOp op) -> op cpu) CPU.init ops

        Assert.That(actual, Is.EqualTo expected)

    [<Test>]
    let TestTranslateDECInstruction () =
        let (CPUOp op) =
            Instruction.DEC RegisterName.A
            |> OpTranslator.translateInstruction

        let expected = 
            CPU.init
            |> CPU.mapRegisterIP (fun _ -> 1)

        let actual = 
            CPU.init
            |> CPU.mapRegisterA (fun _ -> 1)
            |> op 

        Assert.That(actual, Is.EqualTo expected)

    [<Test>]
    let TestTranslateDECInstructionSameReg () =
        let ops =
            [ Instruction.DEC RegisterName.A
            ; Instruction.DEC RegisterName.A
            ; Instruction.DEC RegisterName.A ]
            |> List.map OpTranslator.translateInstruction

        let expected = 
            CPU.init
            |> CPU.mapRegisterIP (fun _ -> 3)

        let actual = 
            List.fold (fun cpu (CPUOp op) -> op cpu) 
                ( CPU.init
                |> CPU.mapRegisterA (fun _ -> 3) ) 
                ops

        Assert.That(actual, Is.EqualTo expected)


    [<Test>]
    let TestTranslateDECInstructionAllRegs () =
        let ops =
            [ Instruction.DEC RegisterName.A
            ; Instruction.DEC RegisterName.B
            ; Instruction.DEC RegisterName.B
            ; Instruction.DEC RegisterName.C
            ; Instruction.DEC RegisterName.C
            ; Instruction.DEC RegisterName.C
            ; Instruction.DEC RegisterName.D
            ; Instruction.DEC RegisterName.D
            ; Instruction.DEC RegisterName.D
            ; Instruction.DEC RegisterName.D ]
            |> List.map OpTranslator.translateInstruction

        let expected = 
            CPU.init
            |> CPU.mapRegisterIP (fun _ -> 10)

        let actual = 
            List.fold (fun cpu (CPUOp op) -> op cpu) 
                ( CPU.init
                |> CPU.mapRegisterA (fun _ -> 1)
                |> CPU.mapRegisterB (fun _ -> 2)
                |> CPU.mapRegisterC (fun _ -> 3)
                |> CPU.mapRegisterD (fun _ -> 4) ) 
                ops

        Assert.That(expected, Is.EqualTo actual)

    [<Test>]
    let TestTranslateJNZInstructionRegSrcNonZero () =
        let (CPUOp op) = 
            Instruction.JNZ (Src.Register RegisterName.A, 10)
            |> OpTranslator.translateInstruction

        let expected =
            CPU.init
            |> CPU.mapRegisterA (fun _ -> 1)
            |> CPU.mapRegisterIP (fun _ -> 10)

        let actual = 
            CPU.init
            |> CPU.mapRegisterA (fun _ -> 1)
            |> op

        Assert.That(actual, Is.EqualTo expected)

    [<Test>]
    let TestTranslateJNZInstructionScalarSrcNonZero () =
        let (CPUOp op) = 
            Instruction.JNZ (Src.Scalar 1, 10)
            |> OpTranslator.translateInstruction

        let expected =
            CPU.init
            |> CPU.mapRegisterIP (fun _ -> 10)

        let actual = 
            CPU.init
            |> op

        Assert.That(actual, Is.EqualTo expected)

    [<Test>]
    let TestTranslateJNZInstructionRegSrcZero () =
        let (CPUOp op) = 
            Instruction.JNZ (Src.Register RegisterName.A, 0)
            |> OpTranslator.translateInstruction

        let expected =
            CPU.init
            |> CPU.mapRegisterA (fun _ -> 0)
            |> CPU.mapRegisterIP (fun _ -> 1)

        let actual = 
            CPU.init
            |> CPU.mapRegisterA (fun _ -> 0)
            |> op

        Assert.That(actual, Is.EqualTo expected)

    [<Test>]
    let TestTranslateJNZInstructionScalarSrcZero () =
        let (CPUOp op) = 
            Instruction.JNZ (Src.Scalar 0, 0)
            |> OpTranslator.translateInstruction

        let expected =
            CPU.init
            |> CPU.mapRegisterIP (fun _ -> 1)

        let actual = 
            CPU.init
            |> op

        Assert.That(actual, Is.EqualTo expected)