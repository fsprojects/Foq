using NUnit.Framework;
using Foq.Linq;

public interface IFoo
{
    bool TryFoo(out int value);
    int GetTwo(ref int one);
}

[TestFixture]
public class ByRefTests
{    
    [Test]
    public void ShouldHandleOutArg()
    {
        int x ;
        var foo = new Mock<IFoo>().Setup(mock => mock.TryFoo(out x)).Returns(true).Create();
        bool success = foo.TryFoo(out x);
        Assert.IsTrue(success);
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

