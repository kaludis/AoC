namespace Assembunny

type CPUOp = CPUOp of (CPU -> CPU)

type CPUOps = CPUOps of CPUOp list

module OpTranslator =

    let private incrementIp = CPU.mapRegisterIP (fun value -> value + 1)

    let private mapRegister = function
        | RegisterName.A -> CPU.mapRegisterA
        | RegisterName.B -> CPU.mapRegisterB
        | RegisterName.C -> CPU.mapRegisterC
        | RegisterName.D -> CPU.mapRegisterD

    let private readRegister = function
        | RegisterName.A -> CPU.readRegisterA
        | RegisterName.B -> CPU.readRegisterB
        | RegisterName.C -> CPU.readRegisterC
        | RegisterName.D -> CPU.readRegisterD


    let translateInstruction instruction =
        match instruction with
        | Instruction.CPY (src, reg) ->
            match src with
            | Src.Register srcReg ->
                (fun cpu -> mapRegister reg (fun _ -> (readRegister srcReg) cpu) cpu)
                >> incrementIp
                |> CPUOp
            | Src.Scalar srcValue ->
                mapRegister reg (fun _ -> srcValue) 
                >> incrementIp
                |> CPUOp
        | Instruction.INC reg ->
            mapRegister reg (fun value -> value + 1) 
            >> incrementIp
            |> CPUOp
        | Instruction.DEC reg ->
            mapRegister reg (fun value -> value - 1) 
            >> incrementIp
            |> CPUOp
        | Instruction.JNZ (src, instructionNumber) ->
            match src with
            | Src.Register srcReg ->
                (fun cpu ->
                    match readRegister srcReg cpu with
                    | 0 -> incrementIp cpu
                    | _ -> CPU.mapRegisterIP (fun ip -> ip + instructionNumber) cpu) |> CPUOp
            | Src.Scalar srcValue ->
                (fun cpu ->
                    match srcValue with
                    | 0 -> incrementIp cpu
                    | _ -> CPU.mapRegisterIP (fun ip -> ip + instructionNumber) cpu) |> CPUOp

    let translate (Program instructions) =
        instructions
        |> List.map translateInstruction
        |> CPUOps

    let tryGetOp index (CPUOps ops) =
        List.tryItem index ops