using NUnit.Framework;
using Foq.Linq;

public interface IFoo
{
    bool TryFoo(out int value);
    bool TryFoo2(out int v1, out string v2);
    int GetTwo(ref int one);
}

[TestFixture]
public class ByRefTests
{    
    [Test]
    public void ShouldHandleOutArg()
    {
        int x = 1;
        var foo = new Mock<IFoo>().Setup(mock => mock.TryFoo(out x)).Returns(true).Create();
        x = 2;
        bool success = foo.TryFoo(out x);
        Assert.IsTrue(success);
        Assert.AreEqual(1, x);
    }

    [Test]
    public void ShouldHandleTwoOutArgs()
    {
        int x = 1;
        string s = "Hello World";
        var foo = new Mock<IFoo>().Setup(mock => mock.TryFoo2(out x, out s)).Returns(true).Create();
        x = -1;
        s = "Not Hello World";
        bool success = foo.TryFoo2(out x, out s);
        Assert.IsTrue(success);
        Assert.AreEqual(1, x);
        Assert.AreEqual("Hello World", s);
    }

    [Test]
    public void ShouldHandleRefArg()
    {
        int one = 1;
        var foo = new Mock<IFoo>().Setup(mock => mock.GetTwo(ref one)).Returns(2).Create();
        int two = foo.GetTwo(ref one);
        Assert.AreEqual(2, two);
    }
}

