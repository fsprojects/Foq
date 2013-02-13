Imports System.Collections.Generic, NUnit.Framework, Foq.Linq

<TestFixture>
Public Class Tests
    <Test>
    Public Sub TestVerifyFunc()
        Dim mock =
            New Mock(Of IList(Of Integer))() _
                .SetupFunc(Function(x) x.Contains(1)).Returns(True) _
                .Create()
        Assert.AreEqual(True, mock.Contains(1))
    End Sub
End Class

