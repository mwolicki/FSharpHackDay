namespace UrhoGame.Lib

open Urho.Resources
open Urho.Gui
open Urho
open Urho.Urho2D
open Urho.Physics
open Urho.Actions
open Dsl


[<AutoOpen>]
module GameKit =

    let log = System.Diagnostics.Debug.WriteLine

    module Sprite =
        let createChild path (scene:Scene) (cache:ResourceCache)  =
            let node = scene.CreateChild("StaticSprite2D/" + path)
            let sprite = node.CreateComponent<StaticSprite2D>()
            sprite.Sprite <- cache.GetSprite2D path

            node

        let setCollisionShape (node:Node) =
            let body = node.CreateComponent<RigidBody>()
            body.Mass <- 1.f
            body.Kinematic <- true
            let collShape = node.CreateComponent<CollisionShape> ();
            collShape.SetSphere (0.3f,Vector3.Zero,Quaternion.Identity);
            node

        let moveActionTo x y (node:Node) =
            let a  = Urho.Actions.MoveTo(3.f, Vector3 (x,y, 0.f)) 
            node.RunActions(a)
            a


        let setCollisionRectangle (size : Vector3) (node:Node) =
            let body = node.CreateComponent<RigidBody>()
            body.Mass <- 1.f
            body.Kinematic <- true
            let collShape = node.CreateComponent<CollisionShape> ();
            collShape.SetBox (size,Vector3.Zero,Quaternion.Identity);
            node

        let inline onCollision f (node:Node) =
            let subscription = node.SubscribeToNodeCollisionStart <| System.Action<_> f
            { new System.IDisposable with member __.Dispose() = subscription.Unsubscribe() }

        let setPosition x y (node:Node) =
            node.Position <- Vector3(x,y,0.f)
            node

        let setScale x y (node:Node) =
            node.Scale <- Vector3(x,y, 0.f)
            node

        let movePosition (pos:Vector2) (node:Node) =
            let currPos = node.Position
            node.Position <- Vector3(currPos.X + (float32 pos.X)/100.f,
                                     currPos.Y - (float32 pos.Y)/100.f,
                                     currPos.Z)
            node

        let movePositionX x (node:Node) = node |> movePosition (Vector2(float32 x, 0.f))
    module Text =
        let getText text = new Text(Value = text, HorizontalAlignment = HorizontalAlignment.Right, VerticalAlignment = VerticalAlignment.Top)
        let setText text (cmpt:Text) = cmpt.Value <- text
        let setAlignment hor vert (cmpt:Text) =
            cmpt.HorizontalAlignment <- hor
            cmpt.VerticalAlignment <- vert
            cmpt 
        let setDefaultFont (cache:ResourceCache) (text:Text) =
            text.SetFont(cache.GetFont("Fonts/Anonymous Pro.ttf"), 15) |> ignore
            text

        let setDefaultFont2 (cache:ResourceCache) size (text:Text) =
            text.SetFont(cache.GetFont("Fonts/Anonymous Pro.ttf"), size) |> ignore
            text

[<AutoOpen>]
module Game =

    type GameState = {
        Time : float32
        Score : uint32
        Pad : Node
        Ball : Node
        Blocks : Set<uint32>
        BallMoveAction: MoveTo option
        Scene : Scene }

    type State =
    | Start of GameState
    | Game of GameState
    | GameOver of lastState:GameState
    | Won of lastState:GameState

    let initializeGame (board : Dsl.BlockSchema) (scene : Scene) (resourceCache : ResourceCache) onColision =
        let drawBlock path x y size=
            
            let x = float32 x * 0.3f + (float32 size * 0.15f) - 3.f
            let y = float32 y * 0.4f + 2.f
            
            let size = (float32 size) * 30.f
            Sprite.createChild path scene resourceCache
             |> Sprite.setPosition x y
             |> Sprite.setScale size 30.f
             |> Sprite.setCollisionRectangle (Vector3 (0.011f, 0.011f, 0.f))
             |> (fun x->x.ID)
        
        let ids = 
         board 
         |> List.collect (fun (elements, pos) -> 
                          let mutable currPos = 0
                          elements |> List.choose (function
                                                  | Block (size,path) -> 
                                                    let d = drawBlock path currPos -pos size
                                                    currPos <- size + currPos
                                                    Some d
                                                  | Space size -> currPos <- size + currPos; None))
         |> Set.ofList

        let pad = Sprite.createChild @"Urho2D/green.png" scene resourceCache
                    |> Sprite.setPosition 0.f -3.f
                    |> Sprite.setScale 100.f 15.f
                    |> Sprite.setCollisionRectangle (Vector3 (0.011f, 0.011f, 0.f))

        let ball = Sprite.createChild "Urho2D/Ball.png" scene resourceCache
                   |> Sprite.setPosition 0.f -2.6f
                   |> Sprite.setCollisionShape


        ball |> Sprite.onCollision onColision |> ignore


        { Time = 180.f
          Score = 0u
          Pad = pad
          Ball = ball
          Blocks = ids
          BallMoveAction = None
          Scene = scene } |> Start


    let startGame ((Start state) : State) padSpeed =
        Game { state with BallMoveAction = state.Ball |> Sprite.moveActionTo padSpeed 4.f |> Some }

type DeviceInput = {
    Move: Input -> IntVector2
    PressedStart: Input -> bool
}

type GameApplication(input: DeviceInput) =

    inherit Application(ApplicationOptions("Data"))
    let mutable textElement : Text = null
    let mutable state : Game.State option = None
    let mutable pendingCollisions = []

    member __.next maxValue (rnd:System.Random) =
        lock rnd (fun () -> rnd.NextDouble() * 2.0 * maxValue - maxValue |> float32)

    override self.Start() =
        
        let scene = new Scene()
        scene.CreateComponent<DebugRenderer>() |> ignore
        scene.CreateComponent<Octree>() |> ignore
        let physicsWorld = scene.CreateComponent<PhysicsWorld2D>()
        physicsWorld.Gravity <- Vector2.Zero
        let camera = scene.CreateChild("Camera")
        camera.Position <- new Vector3(0.0f, 0.0f, -10.0f)

        let camera = camera.CreateComponent<Camera>()
        camera.Orthographic <- true
        camera.OrthoSize <- float32 self.Graphics.Height * 0.01f;

        self.Renderer.SetViewport(0u, new Viewport(self.Context, scene, camera.GetComponent<Camera>(), null))

        let onCollision (args:NodeCollisionStartEventArgs) = pendingCollisions <- args :: pendingCollisions

        state <- Game.initializeGame Dsl.level1 scene self.ResourceCache onCollision |> Some

        
        textElement <- Text.getText "text" |> Text.setDefaultFont self.ResourceCache
        self.UI.Root.AddChild textElement

        let window_menu= new Window()
        self.UI.Root.AddChild window_menu
        let button = new Button()
        button.AddChild (Text.getText "start game" |> Text.setDefaultFont self.ResourceCache)
        window_menu.AddChild button

        

    override self.OnUpdate(timeStep) =
        

        let (|CollisionWithPad|CollisionWithBlock|Unknown|) (s:Game.GameState, args:NodeCollisionStartEventArgs) = 
            if args.OtherNode <> null then
                match args.OtherNode.ID with
                | id when id = s.Pad.ID -> CollisionWithPad
                | id when id = s.Ball.ID -> 
                    if args.Body = null then Unknown
                    else
                        match args.Body.Node.ID with
                        | id when id = s.Pad.ID -> CollisionWithPad
                        | _ -> CollisionWithBlock  args.Body.Node
                | _ -> CollisionWithBlock  args.OtherNode
            else Unknown

        let (|CollisionWithWall|CollistionWithFloor|CollistionWithCeiling|Nothing|) (p:Vector3, a:MoveTo) =
            if p.Y >= 3.f && a.PositionEnd.Y > 0.f then CollistionWithCeiling
            elif p.Y <= - 3.f then CollistionWithFloor
            elif (p.X >= 5.f && a.PositionEnd.X > 0.f) || (p.X <= -5.f && a.PositionEnd.X < 0.f) then CollisionWithWall
            else Nothing


        state <- match state with
                 | Some state -> 
                    (match state with
                     | Game.Start s ->
                        let moveBy = (input.Move self.Input).X
                        s.Pad |> Sprite.movePositionX moveBy |> ignore
                        s.Ball |> Sprite.movePositionX moveBy |> ignore

                        if input.PressedStart self.Input then
                            Game.startGame state (float32 moveBy)
                        else state
                     | Game.Game s when s.Blocks.Count = 0 -> 
                        Won s
                     | Game.Game s ->
                        let s = { s with Time = s.Time - timeStep }
                        let moveBy = (input.Move self.Input).X
                        s.Pad |> Sprite.movePositionX moveBy |> ignore

                        let s= pendingCollisions 
                               |> List.fold(
                                    fun x arg ->
                                            match (x, arg) with
                                            | CollisionWithPad -> 
                                                x.Ball.RemoveAllActions()
                                                let y = abs x.BallMoveAction.Value.PositionEnd.Y
                                                let newBallAction = x.Ball 
                                                                    |> Sprite.moveActionTo (x.BallMoveAction.Value.PositionEnd.X + float32 moveBy) 
                                                                                           y
                                                                    |> Some 
                                                {x with Score = x.Score; BallMoveAction = newBallAction }

                                            | CollisionWithBlock b ->
                                                x.Ball.RemoveAllActions() 
                                                let newBallAction = x.Ball 
                                                                    |> Sprite.moveActionTo (x.BallMoveAction.Value.PositionEnd.X) 
                                                                                           -x.BallMoveAction.Value.PositionEnd.Y
                                                                    |> Some 
                                                let id = b.ID
                                                b.Remove()
                                                {x with Score = x.Score + 10u; BallMoveAction = newBallAction; Blocks = x.Blocks.Remove id }
                                            | Unknown -> 
                                                log (sprintf "UNKNOWN") 
                                                s) s
                        pendingCollisions <- []

                        if textElement <> null then textElement.Value <- sprintf "time %.0f, score: %i" s.Time s.Score 

                        match (s.Ball.Position, s.BallMoveAction.Value) with
                        | CollisionWithWall ->
                            let newBallAction = 
                                s.Ball 
                                |> Sprite.moveActionTo (-s.Ball.Position.X) 
                                                        s.BallMoveAction.Value.PositionEnd.Y
                                |> Some 
                            { s with Score = s.Score; BallMoveAction = newBallAction } |> Game
                        | CollistionWithFloor -> GameOver s
                        | CollistionWithCeiling ->
                            let newBallAction = 
                                s.Ball 
                                |> Sprite.moveActionTo (s.BallMoveAction.Value.PositionEnd.X) -3.f
                                |> Some 
                            {s with Score = s.Score; BallMoveAction = newBallAction } |> Game
                        | Nothing ->
                            if s.Time > 0.f then Game s
                            else GameOver s
                     | GameOver s as state ->
                        s.Ball.RemoveAllActions()

                        textElement <- Text.getText "GAME OVER" 
                                        |> Text.setAlignment HorizontalAlignment.Center VerticalAlignment.Center
                                        |> Text.setDefaultFont2 self.ResourceCache 30
                        self.UI.Root.AddChild textElement
                        state
                     | Won s as state ->
                        s.Ball.RemoveAllActions()

                        textElement <- Text.getText "WIN!!!" 
                                        |> Text.setAlignment HorizontalAlignment.Center VerticalAlignment.Center
                                        |> Text.setDefaultFont2 self.ResourceCache 30
                        self.UI.Root.AddChild textElement
                        state
                     | _ -> state) |> Some
     
                 | None -> state
                 
        System.GC.Collect 0