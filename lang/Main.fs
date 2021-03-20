module File1

open System.IO
open FParsec

open Parsers

[<EntryPoint>]
let main argv =
    //let path = @"C:\Users\Juha Reinikainen\Documents\fsharp\lang\lang\resources\Test3.lang"
    //let text = File.ReadAllText(path)
    let text = """int i=0
    fun tuplaa(a){return 2*a}
    while(i<10){int y=tuplaa(i);print(y);i=i+1}"""

    run pProgram text
    |> printfn "%A"

    0 // return an integer exit code
