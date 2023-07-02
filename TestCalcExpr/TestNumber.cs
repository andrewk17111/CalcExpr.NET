using CalcExpr.Expressions;

namespace TestCalcExpr;

[TestClass]
public class TestNumber
{
    [TestMethod]
    public void TestInit()
    {
        Random random = new Random();

        for (int i = 0; i < 100; i++)
        {
            double val = random.NextDouble() * (Double.MaxValue - Double.MinValue) + Double.MinValue;

            Assert.AreEqual(val, new Number(val).Value);
        }
    }
}
