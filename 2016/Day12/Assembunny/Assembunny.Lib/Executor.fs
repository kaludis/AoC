namespace Assembunny

module Executor =

    open FsToolkit.ErrorHandling

    let rec private exec cpu opCodes =
        match OpTranslator.tryGetOp cpu.IP opCodes with
        | None -> cpu
        | Some (CPUOp opCode) -> exec (opCode cpu) opCodes

    let start cpu listing =
        let execFlow = OpTranslator.translate >> (exec cpu)
        listing
        |> ProgramParser.parseProgram
        |> Result.map execFlow