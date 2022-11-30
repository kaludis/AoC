namespace Assembunny

type CPU =
    { A : int
    ; B : int
    ; C : int 
    ; D : int 
    ; IP : int }

module CPU =

    let init = 
        { CPU.A = 0; B = 0; C = 0; D = 0; IP = 0 }

    let mapRegisterA mapper cpu =
        { cpu with A = mapper cpu.A }

    let mapRegisterB mapper cpu =
        { cpu with B = mapper cpu.B }

    let mapRegisterC mapper cpu =
        { cpu with C = mapper cpu.C }

    let mapRegisterD mapper cpu =
        { cpu with D = mapper cpu.D }

    let mapRegisterIP mapper cpu =
        { cpu with IP = mapper cpu.IP }

    let readRegisterA cpu = cpu.A

    let readRegisterB cpu = cpu.B

    let readRegisterC cpu = cpu.C

    let readRegisterD cpu = cpu.D