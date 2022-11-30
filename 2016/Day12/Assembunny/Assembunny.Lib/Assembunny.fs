namespace Assembunny

type CPU =
    { A : int
    ; B : int
    ; C : int 
    ; D : int }

module CPU =

    let init = 
        { CPU.A = 0; B = 0; C = 0; D = 0 }