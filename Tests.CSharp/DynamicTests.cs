using System;
using System.Collections.Generic;
using System.ComponentModel;
using Microsoft.FSharp.Control;
using NUnit.Framework;
using Foq.Linq;

public enum Side { Bid, Ask }
public enum TimeInForce { GoodForDay, GoodTillCancel }

public interface IOrder
{
    decimal Price { get; }
    int Quantity { get; }
    Side Side { get; }
    TimeInForce TimeInForce { get; }
}

[TestFixture]
public class DynamicTests
{
    [Test]
    public void TestProperySetupViaAnonymousClass()
    {
        var order =
            new Mock<IOrder>()
                .SetupProperties(new {
                    Price = 99.99M,
                    Quantity = 10,
                    Side = Side.Bid,
                    TimeInForce = TimeInForce.GoodTillCancel
                })
            .Create();
        Assert.AreEqual(99.99M, order.Price);
        Assert.AreEqual(10, order.Quantity);
        Assert.AreEqual(Side.Bid, order.Side);
        Assert.AreEqual(TimeInForce.GoodTillCancel, order.TimeInForce);
    }
}

