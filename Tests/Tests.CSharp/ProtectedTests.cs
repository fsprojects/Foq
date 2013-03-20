using NUnit.Framework;
using Foq.Linq;

public abstract class AbstractBaseClass
{
    public abstract bool AbstractMethod();
}

public abstract class ClassWithProtectedMember : AbstractBaseClass
{
    abstract protected int ProtectedMember();

    public int Accessor()
    {
        return ProtectedMember();
    }

    public sealed override bool AbstractMethod()
    {
        return true;
    }

    public bool ConcreteMethod()
    {
        return true;
    }
}

[TestFixture]
class ProtectedTests
{
    [Test]
    public void TestProtectedMemberAccess()
    {
        var mock =
            new Mock<ClassWithProtectedMember>()
                .SetupByName<int>("ProtectedMember").Returns(1)
                .Create();

        Assert.AreEqual(1, mock.Accessor());
    }

    [Test]
    public void TestSealedMethod()
    {
        var mock = Mock.Of<ClassWithProtectedMember>();
        Assert.IsTrue(mock.AbstractMethod());
    }

    [Test]
    public void TestConcreteMethod()
    {
        var mock = Mock.Of<ClassWithProtectedMember>();
        Assert.IsTrue(mock.ConcreteMethod());
    }
}

