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
                .SetupAction(x => x.Clear()).Raises<System.ApplicationException>()
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
                .SetupAction(x => x.Clear()).Raises(new System.ApplicationException(message))
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
                .SetupPropertySet(x => x[1]).Raises<System.ApplicationException>()
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
}
