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

    [TestMethod]
    public void TestParse()
    {
        /*
         * Parser returns a valid Number with it's value equal to the number represented in the strings using the
         * following requirements.
         * 1. Only numeric digits and periods
         * 2. Optional leading '.'
         * 3. Optional trailing '.'
         * 4. Optional leading 0's
         * 5. Optional trailing 0's
         */

        Assert.Fail();
    }

    [TestMethod]
    public void TestConvert()
    {
        /*
         * Number can cast to decimal, double, float, long, int, short, sbyte, ulong, uint, ushort, and byte for valid
         * values.
         */

        Assert.Fail();
    }
}
