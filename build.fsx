#r "paket:
nuget Fake.DotNet.Cli
nuget Fake.Core.Target
nuget Fake.JavaScript.Npm //"
#load "./.fake/build.fsx/intellisense.fsx"

open Fake.Core
open Fake.IO
open Fake.DotNet


let pluginId = "com.garethhubball.streamdeckfs"
let projectName = "streamdeckfs.fsproj"

let appData = Environment.environVar("APPDATA")

let assetsDir =  "./assets/"
let pluginSourceDir = "./plugin/"
let propertyInspectorBaseDir = "./property-inspector/"
let propertyInspectorBuildDir = Path.combine propertyInspectorBaseDir "public"

let buildDir = "./build/"
let pluginDir = sprintf "%s/Elgato/StreamDeck/Plugins/%s.sdPlugin" appData pluginId
let propertyInspectorDir = Path.combine pluginDir "property-inspector"

let sdProcess =
    Process.getAllByName("StreamDeck")
    |> Seq.filter (fun p -> p.ProcessName = "StreamDeck")
    |> Seq.tryHead

let streamdeckFilename = Option.map Process.getFileName sdProcess

let rec cleanDir (dir: string) =
    try
        Shell.cleanDir dir
    with
    | _ ->
        System.Threading.Thread.Sleep(100)
        cleanDir dir

Target.create "Clean" (fun _ -> 
    Shell.cleanDir buildDir
)

Target.create "BuildApp" (fun _ ->
    DotNet.build (fun options ->
        { options with OutputPath = Some(buildDir); Configuration = DotNet.BuildConfiguration.Debug }) (Path.combine pluginSourceDir projectName)
)

Target.create "CopyAssetsToBuildDir" (fun _ ->
    Shell.copyDir buildDir assetsDir (fun _ -> true)
)

Target.create "CopyPlugin" (fun _ ->
    cleanDir pluginDir
    Shell.copyDir pluginDir buildDir (fun _ -> true)
)

Target.create "CopyPropertyInspector" (fun _ -> 
    cleanDir propertyInspectorDir
    Shell.copyDir propertyInspectorDir propertyInspectorBuildDir (fun _ -> true) 
)

Target.create "ShutStreamDeck" (fun _ ->
    sdProcess |> Option.iter (fun p ->
        p.Kill()
        p.WaitForExit()
    )
)

Target.create "OpenStreamDeck" (fun _ ->
    streamdeckFilename |> Option.iter (fun filename ->
        Process.setKillCreatedProcesses(false) 
        CreateProcess.fromRawCommand filename []
        |> Proc.start
        |> ignore
    )
)

Target.create "BuildPropertyInspector" (fun _ ->
    Fake.JavaScript.Npm.exec "run webpack" (fun o -> { o with WorkingDirectory = propertyInspectorBaseDir })
)

Target.create "Plugin" ignore
Target.create "PropertyInspector" ignore
Target.create "Full" ignore

open Fake.Core.TargetOperators

"Clean"
    ==> "BuildApp"
    ==> "CopyAssetsToBuildDir"
    ?=> "ShutStreamDeck"
    ==> "CopyPlugin"
    ?=> "CopyPropertyInspector"
    ==> "OpenStreamDeck"
    ==> "Plugin"
    ==> "Full"

"CopyAssetsToBuildDir"
    ==> "CopyPlugin"
    ==> "Plugin"


"Clean"
    ==> "BuildPropertyInspector"
    ?=> "ShutStreamDeck"
    ==> "CopyPropertyInspector"
    ==> "OpenStreamDeck"
    ==> "PropertyInspector"
    ==> "Full"

"BuildPropertyInspector"
    ==> "PropertyInspector"

// start build
Target.runOrDefault "Full"