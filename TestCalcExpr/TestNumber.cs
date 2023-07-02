using CalcExpr.Expressions;
using CalcExpr.Parsing;

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
        Dictionary<string, double> expressions = new Dictionary<string, double>()
        {
            { ".1234", 0.1234 },
            { "0.1234", 0.1234 },
            { "1234.", 1234 },
            { "1234", 1234 },
            { "436.246", 436.246 },
            { "0123", 123 },
            { "1230", 1230 },
            { "489.38400", 489.384 }
        };

        foreach (string expression in expressions.Keys)
            Assert.AreEqual (((Number)new Parser().Parse(expression)).Value, expressions[expression]);
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
