/// ATM example based on code in Alexander Chaffee & William Pietri's article
/// Unit testing with mock objects: http://www.ibm.com/developerworks/library/j-mocktest/index.html
module ``ATM Example``

open System
open System.Globalization

type Transaction =
    abstract SetSourceAccount : Account -> unit
    abstract SetDestAccount : Account -> unit
    abstract SetAmount : decimal -> unit
    abstract Process : unit -> unit
    abstract Successful : bool
and Account = interface end

let TEST_CASH_ACCOUNT = { new Account }
let TEST_CHECKING_ACCOUNT = { new Account }

type AtmGui(createTransaction:unit->Transaction) =
    let mutable userAccount = None
    let mutable enteredAmount = None
    let mutable display = ""
    let dispense amount = ()
    let myCashAccount () = TEST_CASH_ACCOUNT
    let doWithdrawal(account:Account, amount:decimal) =
        let transaction = createTransaction()
        transaction.SetSourceAccount(account)
        transaction.SetDestAccount(myCashAccount())
        transaction.SetAmount(amount)
        transaction.Process()
        if transaction.Successful then dispense(amount)
    let doAction() =
        match userAccount, enteredAmount with
        | Some account, Some amount -> doWithdrawal(account, amount)
        | None, _ -> invalidOp "No account"
        | _, None -> invalidOp "No amount"
    member atm.SetAccount(account) = 
        userAccount <- Some account
    member atm.PressButton(button) =
        if button = "Continue" then doAction()
    member atm.PressButtons([<ParamArray>] digits:string[]) =
        let entered = String.Join("",digits)
        let amount = Decimal.Parse(entered) / 100M
        enteredAmount <- Some amount
        let us = CultureInfo.CreateSpecificCulture("en-US")
        display <- amount.ToString("C", us)
    member atm.GetDisplayContents() =
        display

open NUnit.Framework
open Foq

let insertCardAndInputPin(atm:AtmGui) =
    atm.SetAccount(TEST_CHECKING_ACCOUNT)

let [<Test>] ``test checking withdrawal`` () =
    let mockTransaction = Mock.Of<Transaction>()
    let atm = new AtmGui(fun () -> mockTransaction)    

    insertCardAndInputPin(atm)

    atm.PressButton("Withdraw")
    atm.PressButton("Checking")
    atm.PressButtons("1", "0", "0", "0", "0")
    Assert.AreEqual("$100.00", atm.GetDisplayContents());
    atm.PressButton("Continue")

    Mock.Verify(<@ mockTransaction.SetAmount(100M) @>)
    Mock.Verify(<@ mockTransaction.SetSourceAccount(TEST_CHECKING_ACCOUNT) @>)
    Mock.Verify(<@ mockTransaction.SetDestAccount(TEST_CASH_ACCOUNT) @>)    