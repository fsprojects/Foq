// Code based on samples in Russel Allen's 
// Unit Testing Good Patterns #3 - Know Your Moq Argument Matchers
// http://russellallen.info/post/2011/06/29/Unit-Testing-Good-Patterns-3-Know-Your-Moq-Argument-Matchers!.aspx
module TeaExample

type CupOfTea () = class end

type ITeaBag =
    abstract CupOfTea: CupOfTea with get, set
    abstract Brew: unit -> unit

let Provide(teabag:ITeaBag) =
  let cupOfTea = CupOfTea()
  teabag.CupOfTea <- cupOfTea
  teabag.Brew()
  cupOfTea

open Foq
open NUnit.Framework

let [<Test>] Provide_TeaBagIsBrewed() =
  let teabag = Mock.Of<ITeaBag>()
  let cuppa = Provide(teabag)
  verify <@ teabag.Brew() @> once




