using NUnit.Framework;
using Foq.Linq;

public abstract class AbstractBaseClass
{
    public abstract int AbstractMethod();
}

public abstract class ClassWithProtectedMember : AbstractBaseClass
{
    abstract protected int ProtectedMember();

    public int Accessor()
    {
        return ProtectedMember();
    }

    public sealed override int AbstractMethod()
    {
        throw new System.NotImplementedException();
    }
}

[TestFixture]
class ProtectedTests
{
    [Test]
    public void TestAccess()
    {
        var mock =
            new Mock<ClassWithProtectedMember>()
                .SetupByName<int>("ProtectedMember").Returns(1)
                .Create();

        Assert.AreEqual(1, mock.Accessor());
    }
}

