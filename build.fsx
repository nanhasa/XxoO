#r "paket:
nuget Fake.Core.Target
nuget Fake.DotNet.Cli
nuget Fake.IO.FileSystem
nuget FSharp.Literate  //"
#load ".fake/build.fsx/intellisense.fsx"

open System.IO
open Fake.Core
open Fake.IO
open FSharp.Markdown

let markdown = "./privacy_statement.md" |> Path.getFullName

Target.create "ConvertMarkdown" (fun _ ->
    File.ReadAllText markdown
    |> Markdown.Parse
    |> Markdown.WriteHtml
    |> fun content -> File.WriteAllText("index.html", content)
)

"ConvertMarkdown"

Target.runOrDefault "ConvertMarkdown"