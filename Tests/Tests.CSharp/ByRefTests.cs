using NUnit.Framework;
using Foq.Linq;

public interface IFoo
{
    bool TryFoo(out int value);
}

[TestFixture]
public class ByRefTests
{
    [Test]
    public void ShouldHandleByRefArg()
    {
        int x ;
        var foo = new Mock<IFoo>().Setup(mock => mock.TryFoo(out x)).Returns(true).Create();
        bool success = foo.TryFoo(out x);
        Assert.IsTrue(success);
    }
}

