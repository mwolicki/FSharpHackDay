namespace UrhoGame.Mac
open UrhoGame.Lib


module main =
    [<EntryPoint>]
    let main args =
        use app = new GameApplication({ Move = fun input -> input.MouseMove
                                        PressedStart = fun i -> i.GetKeyPress Urho.Key.Space })
        app.Run()