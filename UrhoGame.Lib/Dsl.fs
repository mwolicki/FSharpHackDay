namespace UrhoGame.Lib

    module Dsl = 
        type BlockLine =
          | Block of count:int * path:string
          | Space of count:int

        type Line = BlockLine list * int

        type BlockSchema = Line list


        let (|IsBeginBlock|_|) (s:string) = 
            match s.[0] with
            | 'B' -> Some @"Urho2D/blue.png"
            | 'G' -> Some @"Urho2D/green.png"
            | 'R' -> Some @"Urho2D/red.png"
            | 'P' -> Some @"Urho2D/purple.png"
            | _-> None 

        let (|IsSpace|_|) (s:string) = if s.[0] = ' ' then Some() else None


        let parse s =
          let howLong (s:string) ch = s |> Seq.cast<char> |> Seq.takeWhile (fun x-> x = ch) |> Seq.length
          let rec parse' remainingString =
              match remainingString with
              | "" -> []
              | IsBeginBlock path ->
                let len = howLong (remainingString.Substring(1, remainingString.Length - 1)) 'x'
                let len = len + 1 
                Block (len, path) :: parse' (remainingString.Substring(len, remainingString.Length - len))
              | IsSpace -> 
                let len = howLong remainingString ' '
                Space len :: parse' (remainingString.Substring(len, remainingString.Length - len))
              
              | s -> sprintf "Shouldn't happened..... %A" s |> failwith
          s |> List.mapi (fun i x -> parse' x, i)

        let level1 : BlockSchema = 
            [ "Bxx RG BPRGPxxxxxxxx"
              "RG Pxxxxxxx P Gxxxxx"
              "BxxxxxxxxxxPxx RG BP" ] |> parse
        let level0 : BlockSchema = 
            [ "Bxxxxxxxxxxxxx" ] |> parse
        
