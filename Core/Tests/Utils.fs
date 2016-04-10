namespace Ufrgs.Inf.ArchSims.Core.Tests

open Microsoft.VisualStudio.TestTools.UnitTesting

module Utils = 
    let equals expected actual = Assert.AreEqual(expected, actual)

    let (|>==) a b = a |> equals b
     
