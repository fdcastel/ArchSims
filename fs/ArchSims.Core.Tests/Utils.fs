namespace Ufrgs.Inf.ArchSims.Core.Tests

open NUnit.Framework

module Utils = 
    let equals expected actual = Assert.AreEqual(expected, actual)

    let (|>==) a b =
        a |> equals b 

    let equalsArr a b =
        Array.compareWith (fun a b -> if a = b then 0 else 1) a b |> equals 0