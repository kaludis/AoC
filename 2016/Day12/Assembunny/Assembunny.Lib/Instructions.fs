namespace Assembunny

[<RequireQualifiedAccess>]
type RegisterName =
    | A
    | B
    | C
    | D

[<RequireQualifiedAccess>]
type Src =
    | Register of RegisterName
    | Scalar of int

[<RequireQualifiedAccess>]
type Instruction =
    | CPY of (Src * RegisterName)
    | INC of RegisterName
    | DEC of RegisterName
    | JNZ of (Src * int)