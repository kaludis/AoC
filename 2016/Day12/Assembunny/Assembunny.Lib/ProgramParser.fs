namespace Assembunny

module ProgramParser =

    [<RequireQualifiedAccess>]
    type ParseError =
        | BadSrcOperand of string
        | BadRegisterOperand of string
        | BadValueOperand of string
        | BadInstruction of string

    open FsToolkit.ErrorHandling

    let private tryParseRegName regName =
        match regName with
        | "a" -> Ok RegisterName.A
        | "b" -> Ok RegisterName.B
        | "c" -> Ok RegisterName.C
        | "d" -> Ok RegisterName.D
        | _ -> Error <| ParseError.BadRegisterOperand $"Bad register operand name '{regName}'"

    let private tryParseInt (value :string) =
        match System.Int32.TryParse(value) with
        | true, n -> Ok n
        | _ -> Error <| ParseError.BadValueOperand $"Bad integer operand value '{value}'"

    let private tryParseSrc (src : string) =
        match tryParseInt src, tryParseRegName src with
        | Ok _, Ok _ -> Error <| ParseError.BadSrcOperand $"Bad src operand '{src}'"
        | Ok value, Error _ -> Ok <| Src.Scalar value
        | Error _, Ok value -> Ok <| Src.Register value
        | Error _, Error _ -> Error <| ParseError.BadSrcOperand $"Bad src operand '{src}'"

    let private parseLine (line : string) =
        match line.ToLower().Split(' ') with
        | [|"cpy"; src; regName|] ->
            result {
                let! srcOperand = tryParseSrc src
                let! regOperand = tryParseRegName regName
                return Instruction.CPY (srcOperand, regOperand)
            }
        | [|"inc"; regName|] ->
            result {
                let! regOperand = tryParseRegName regName
                return Instruction.INC regOperand
            }
        | [|"dec"; regName|] ->
            result {
                let! regOperand = tryParseRegName regName
                return Instruction.DEC regOperand
            }
        |[|"jnz"; src; value|] ->
            result {
                let! srcOperand = tryParseSrc src
                let! instructionNumber = tryParseInt value
                return Instruction.JNZ (srcOperand, instructionNumber)
            }
        | _ -> Error <| ParseError.BadInstruction $"Mailformed instruction '{line}'"

    let parseProgram (lines : string list) = 
        lines
        |> List.map parseLine
        |> List.sequenceResultA
        |> Result.map Program