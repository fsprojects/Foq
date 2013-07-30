using System;
using NUnit.Framework;
using Foq.Linq;

public interface MyInterface
{
    bool Foo(int x);
}

public abstract class MyAbstractBaseClass : MyInterface
{
    public abstract bool Bar(int x);

    public bool Foo(int x)
    {
        return false;
    }
}

public abstract class MyAbstractClass : MyAbstractBaseClass
{
    public override bool Bar(int x)
    {
        throw new NotImplementedException();
    }

    public abstract bool Far(int x);
        
}

[TestFixture]
public class InterfaceTests
{
    [Test]
    public void CanMockAbstractBaseClassWithInterfaceImplementation()
    {
        var mock = new Mock<MyAbstractBaseClass>().Setup(x => x.Bar(1)).Returns(true).Create();
        Assert.AreEqual(true, mock.Bar(1));
    }

    [Test]
    public void CanMockAbstractClassWithInterfaceImplemention()
    {
        var mock = new Mock<MyAbstractClass>().Setup(x => x.Far(1)).Returns(true).Create();
        Assert.AreEqual(true, mock.Far(1));
    }
}
