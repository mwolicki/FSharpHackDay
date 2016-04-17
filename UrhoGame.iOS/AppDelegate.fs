namespace UrhoHame.iOS

open System

open UIKit
open Urho.iOS
open Foundation
open CoreMotion
open UrhoGame.Lib

[<Register ("AppDelegate")>]
type AppDelegate () =
    inherit UIApplicationDelegate ()
    let manager = new CMMotionManager(AccelerometerUpdateInterval = 1./10.)

    override this.FinishedLaunching (app, options) =
        let syncContext = System.Threading.SynchronizationContext.Current

        let mutable x = 0
        let mutable y = 0
        
        manager.StartAccelerometerUpdates (NSOperationQueue.CurrentQueue, 
            CMAccelerometerHandler(fun data error -> x <- data.Acceleration.X * 6. |> int
                                                     y <- data.Acceleration.Y * 6. |> int))

        async{
             do! Async.SwitchToContext syncContext
             let app = new GameApplication({ Move = fun _ -> Urho.IntVector2 (y, x)  })
             app.Run() |> ignore 
        } |> Async.Start

        app.IdleTimerDisabled <- true
       
        true
