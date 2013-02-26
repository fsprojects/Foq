using System;
using System.Collections.Generic;
using System.ComponentModel;
using Microsoft.FSharp.Control;
using NUnit.Framework;
using Foq.Linq;

[TestFixture]
public class LinqTests
{       
    [Test]
    public void TestSetupFunc()
    {
        var mock =
            new Mock<IList<int>>()
                .SetupFunc(x => x.Contains(It.IsAny<int>())).Returns(true)
                .Create();
        Assert.IsTrue(mock.Contains(1));
    }

    [Test]
    public void TestSetupFuncWithReturnsLambda()
    {
        var mock =
            new Mock<IList<int>>()
                .SetupFunc(x => x.Contains(It.IsAny<int>())).Returns(() => true)
                .Create();
        Assert.IsTrue(mock.Contains(1));
    }

    [Test]
    public void TestSetupAction()
    {
        var mock =
            new Mock<IList<int>>()
                .SetupAction(x => x.Clear()).Raises<ApplicationException>()
                .Create();
        Assert.Throws<ApplicationException>(
            () => mock.Clear()
        );
    }

    [Test]
    public void TestSetupActionWithRaisesExceptionValue()
    {
        var message = "Message";
        var mock =
            new Mock<IList<int>>()
                .SetupAction(x => x.Clear()).Raises(new ApplicationException(message))
                .Create();
        Assert.Throws<ApplicationException>(
            () => mock.Clear(),
            message
        );
    }

    [Test]
    public void TestSetupPropertyGet()
    {
        var mock =
            new Mock<IList<int>>()
                .SetupPropertyGet(x => x.Count).Returns(1)
                .Create();
        Assert.AreEqual(1, mock.Count);
    }

    [Test,Combinatorial]
    public void TestSetupPropertyGetWithIndexParameters()
    {
        var value = 9;
        var index = 1;
        var mock =
            new Mock<IList<int>>()
                .SetupPropertyGet(x => x[index]).Returns(value)
                .Create();
        Assert.AreEqual(value, mock[index]);
    }

    [Test]
    public void TestSetupPropertySet()
    {
        var mock =
            new Mock<IList<int>>()
                .SetupPropertySet(x => x[1]).Raises<ApplicationException>()
                .Create();
        Assert.Throws<ApplicationException>(() =>
            mock[1] = 1
        );
    }

    [Test]
    public void TestSetupEvent()
    {
        var fe = new FSharpEvent<PropertyChangedEventHandler, PropertyChangedEventArgs>();
        var mock =
            new Mock<INotifyPropertyChanged>()
                .SetupEvent("PropertyChanged").Publishes(fe.Publish)
                .Create();
        var triggered = false;
        var name = "Name";
        mock.PropertyChanged += (s, e) => triggered = (e.PropertyName == name);
        fe.Trigger(this, new PropertyChangedEventArgs(name));
        Assert.IsTrue(triggered);
    }

    [Test]
    public void TestStrictMode()
    {
        var mock = new Mock<IList<int>>(MockMode.Strict).Create();
        Assert.Throws<NotImplementedException>(() => { var _ = mock.Count; });
    }

    [Test]
    public void TestLooseMode()
    {
        var mock = new Mock<IList<int>>(MockMode.Loose).Create();
        Assert.AreEqual(default(int), mock.Count);
    }

    [Test]
    public void TestExpectFunc()
    {
        var list = new Mock<IList<int>>().SetupFunc(mock => mock.Contains(1)).Returns(true).Create();
        Mock.Expect(() => list.Contains(1), Times.Once);
        var _ = list.Contains(1);
    }

    [Test]
    public void TestVerifyFunc()
    {
        var list = new Mock<IList<int>>().SetupFunc(mock => mock.Contains(1)).Returns(true).Create();
        var _ = list.Contains(1);
        Mock.Verify(() => list.Contains(1));
    }

    [Test]
    public void TestExpectAction()
    {
        var list = Mock.Of<IList<int>>();
        Mock.Expect(() => list.Clear(), Times.Once);
        list.Clear();
    }

    [Test]
    public void TestVerifyAction()
    {
        var list = Mock.Of<IList<int>>();
        list.Clear();
        Mock.Verify(() => list.Clear());
    }

    [Test]
    public void TestVerifyPropertyGet()
    {
        var list = Mock.Of<IList<int>>();
        var count = list.Count;
        Mock.VerifyPropertyGet(() => list.Count);
    }

    [Test]
    public void TestVerifyPropertyGetWithIndexParameters()
    {
        var index = 1;
        var mock = Mock.Of<IList<int>>();
        var value = mock[index];
        Mock.VerifyPropertyGet(() => mock[index]);
    }

    [Test]
    public void TestVerifyPropertySetWithIndexParameters()
    {
        var index = 1;
        var mock = Mock.Of<IList<int>>();
        mock[index] = 9;
        Mock.VerifyPropertySet(() => mock[index]);
    }

    public interface ISettable
    {
        int Value { get; set; }
    }

    [Test]
    public void TestVerifyPropertySet()
    {
        var mock =
            new Mock<ISettable>(MockMode.Loose).Create();
        mock.Value = 1;
        Mock.VerifyPropertySet(() => mock.Value);
    }
}