Imports System, System.Collections.Generic, System.ComponentModel
Imports NUnit.Framework, Foq.Linq

<TestFixture>
Public Class Tests
    <Test>
    Public Sub TestSetupFunc()
        Dim list =
            New Mock(Of IList(Of Integer))() _
                .SetupFunc(Function(mock) mock.Contains(1)).Returns(True) _
                .Create()
        Assert.AreEqual(True, list.Contains(1))
    End Sub
    <Test>
    Public Sub TestSetupFuncWithAnyIntegerArgument()
        Dim list =
            New Mock(Of IList(Of Integer))() _
                .SetupFunc(Function(mock) mock.Contains(It.IsAny(Of Integer)())).Returns(True) _
                .Create()
        Assert.AreEqual(True, list.Contains(1))
    End Sub
    <Test>
    Public Sub TestSetupFuncWithLambdaArgument()
        Dim list =
            New Mock(Of IList(Of Integer))() _
                .SetupFunc(Function(mock) mock.Contains(It.Is(Of Integer)(Function(x) x >= 0))).Returns(True) _
                .Create()
        Assert.AreEqual(True, list.Contains(1))
        Assert.AreEqual(False, list.Contains(-1))
    End Sub
    <Test>
    Public Sub TestSetupFuncWithAReturnsLambda()
        Dim list =
            New Mock(Of IList(Of Integer))() _
                .SetupFunc(Function(mock) mock.Contains(1)).Returns(Function() True) _
                .Create()
        Assert.AreEqual(True, list.Contains(1))
    End Sub
    <Test>
    Public Sub TestSetupSub()
        Dim list =
            New Mock(Of IList(Of Integer))() _
                .SetupSub(Sub(mock) mock.Add(1)).Raises(Of ApplicationException) _
                .Create()
        Assert.Throws(Of ApplicationException)(Sub() list.Add(1))
    End Sub
    <Test>
    Public Sub TestSetupPropertyGet()
        Dim list =
            New Mock(Of IList(Of Integer))() _
                .SetupPropertyGet(Function(mock) mock.Count).Returns(1) _
                .Create()
        Assert.AreEqual(1, list.Count)
    End Sub
    <Test>
    Public Sub TestSetupPropertyIndexer()
        Dim list =
            New Mock(Of IList(Of Integer))() _
                .SetupPropertyGet(Function(mock) mock(1)).Returns(1) _
                .Create()
        Assert.AreEqual(1, list(1))
    End Sub
    <Test>
    Public Sub TestSetupEvent()
        Dim e = New Microsoft.FSharp.Control.FSharpEvent(Of PropertyChangedEventHandler, PropertyChangedEventArgs)()
        Dim instance =
            New Mock(Of INotifyPropertyChanged)() _
                .SetupEvent("PropertyChanged").Publishes(e.Publish) _
                .Create()
        Dim triggered = False
        AddHandler instance.PropertyChanged, Sub() triggered = True
        e.Trigger(Me, New PropertyChangedEventArgs("Name"))
        Assert.AreEqual(True, triggered)
    End Sub
    <Test>
    Public Sub TestExpectSub()
        Dim list = Mock.Of(Of IList(Of Integer))()
        Mock.ExpectSub(Sub() list.Add(1), Times.Once)
        list.Add(1)
    End Sub
    <Test>
    Public Sub TestVerifySub()
        Dim list = Mock.Of(Of IList(Of Integer))()
        list.Add(1)
        Mock.Verify(Sub() list.Add(1))
    End Sub
    <Test>
    Public Sub TestExpectFunc()
        Dim list = Mock.Of(Of IList(Of Integer))()
        Mock.Expect(Function() list.Contains(1), Times.Once)
        list.Contains(1)
    End Sub
    <Test>
    Public Sub TestVerifyFunc()
        Dim list = Mock.Of(Of IList(Of Integer))()
        list.Contains(1)
        Mock.Verify(Function() list.Contains(1))
    End Sub
    <Test>
    Public Sub TestVerifyPropertyGet()
        Dim list = Mock.Of(Of IList(Of Integer))()
        Dim count = list.Count
        Mock.VerifyPropertyGet(Function() list.Count)
    End Sub
    <Test>
    Public Sub TestSetupProperties()
        Dim list =
            New Mock(Of IList(Of Integer))() _
                .SetupProperties(New With {.Count = 1}) _
                .Create()
        Assert.AreEqual(1, list.Count)
    End Sub
    Public Interface IOrder
        Property Price() As Decimal
        Property Quantity() As Integer
    End Interface
    <Test>
    Public Sub TestSetupPropertiesTwo()
        Dim order =
            New Mock(Of IOrder)() _
                .SetupProperties(New With {.Price = 9.99D, .Quantity = 10}) _
                .Create()
        Assert.AreEqual(9.99, order.Price)
        Assert.AreEqual(10, order.Quantity)
    End Sub
    <Test>
    Public Sub TestExpectPropertySet()
        Dim order = Mock.Of(Of IOrder)()
        Mock.ExpectPropertySet(Function() order.Price, Times.Once)
        order.Price = 1
    End Sub
    <Test>
    Public Sub TestVerifyPropertySet()
        Dim order = Mock.Of(Of IOrder)()
        order.Price = 1
        Mock.VerifyPropertySet(Function() order.Price)
    End Sub
End Class