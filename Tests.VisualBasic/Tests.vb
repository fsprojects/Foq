Imports System, System.Collections.Generic, NUnit.Framework, Foq.Linq

<TestFixture>
Public Class Tests
    <Test>
    Public Sub TestSetupFunc()
        Dim mock =
            New Mock(Of IList(Of Integer))() _
                .SetupFunc(Function(x) x.Contains(1)).Returns(True) _
                .Create()
        Assert.AreEqual(True, mock.Contains(1))
    End Sub
    <Test>
    Public Sub TestSetupAction()
        Dim mock =
            New Mock(Of IList(Of Integer))() _
                .SetupAction(Sub(x) x.Add(1)).Raises(Of ApplicationException) _
                .Create()
        Assert.Throws(Of ApplicationException)(Sub() mock.Add(1))
    End Sub
    <Test>
    Public Sub TestSetupPropertyGet()
        Dim mock =
            New Mock(Of IList(Of Integer))() _
                .SetupPropertyGet(Function(x) x.Count).Returns(1) _
                .Create()
        Assert.AreEqual(1, mock.Count)
    End Sub
    <Test>
    Public Sub TestSetupPropertyIndexer()
        Dim mock =
            New Mock(Of IList(Of Integer))() _
                .SetupPropertyGet(Function(x) x(1)).Returns(1) _
                .Create()
        Assert.AreEqual(1, mock(1))
    End Sub
End Class

