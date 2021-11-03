module Utilities

open System.IO

let GetLinesFromFile(path: string) =
    File.ReadLines(__SOURCE_DIRECTORY__ + @"../../" + path)