﻿using System;
using System.Collections.Generic;
using System.ComponentModel;
using Microsoft.FSharp.Control;
using NUnit.Framework;
using Foq.Linq;

[TestFixture]
public class LinqTests
{
    [Test]
    public void TestFunc()
    {
        var mock =
            new Mock<IList<int>>()
                .SetupFunc(x => x.Contains(It.IsAny<int>())).Returns(true)
                .Create();
        Assert.IsTrue(mock.Contains(1));
    }

    [Test]
    public void TestAction()
    {
        var mock =
            new Mock<IList<int>>()
                .SetupAction(x => x.Clear()).Raises<System.ApplicationException>()
                .Create();
        Assert.Throws<ApplicationException>(() =>
            mock.Clear()
        );
    }

    [Test]
    public void TestPropertyGet()
    {
        var mock =
            new Mock<IList<int>>()
                .SetupPropertyGet(x => x.Count).Returns(1)
                .Create();
        Assert.AreEqual(1, mock.Count);
    }

    [Test]
    public void TestPropertySet()
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
    public void TestEvent()
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
