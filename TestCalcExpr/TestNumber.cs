using CalcExpr.Expressions;
using CalcExpr.Parsing;

namespace TestCalcExpr;

[TestClass]
public class TestNumber
{
    /// <summary>
    /// Tests that the value of the Number is the same as the value passed into the Number's constructor.
    /// </summary>
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

    /// <summary>
    /// Tests that a number can be parsed with an optional leading decimal point, optional trailing decimal point,
    /// optional leading and trailing zeroes, and that the value represented in the string is the same value of the
    /// parsed Number.
    /// </summary>
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

    /// <summary>
    /// Tests that the Number can be cast to and from decimal, double, float, long, int, short, sbyte, ulong, uint,
    /// ushort, and byte.
    /// </summary>
    [TestMethod]
    public void TestConvert()
    {
        Assert.AreEqual(0.156489m, (decimal)new Number((double)0.156489m));
        Assert.AreEqual(0.24567841318, (double)new Number(0.24567841318));
        Assert.AreEqual(-5648.0549f, (float)new Number(-5648.0549f));
        Assert.AreEqual(-123157L, (long)new Number(-123157L));
        Assert.AreEqual(-1234853, (int)new Number(-1234853));
        Assert.AreEqual((short)-4364, (short)new Number((short)-4364));
        Assert.AreEqual((sbyte)-127, (sbyte)new Number((sbyte)-127));
        Assert.AreEqual(123475UL, (ulong)new Number(123475UL));
        Assert.AreEqual(1237488U, (uint)new Number(1237488U));
        Assert.AreEqual((ushort)4364, (ushort)new Number((ushort)4364));
        Assert.AreEqual((byte)255, (byte)new Number((byte)255));

        Assert.AreEqual(new Number((double)decimal.MaxValue).Value, ((Number)decimal.MaxValue).Value);
        Assert.AreEqual(new Number(double.MaxValue).Value, ((Number)double.MaxValue).Value);
        Assert.AreEqual(new Number(float.MaxValue).Value, ((Number)float.MaxValue).Value);
        Assert.AreEqual(new Number(long.MaxValue).Value, ((Number)long.MaxValue).Value);
        Assert.AreEqual(new Number(int.MaxValue).Value, ((Number)int.MaxValue).Value);
        Assert.AreEqual(new Number(short.MaxValue).Value, ((Number)short.MaxValue).Value);
        Assert.AreEqual(new Number(sbyte.MaxValue).Value, ((Number)sbyte.MaxValue).Value);
        Assert.AreEqual(new Number(ulong.MaxValue).Value, ((Number)ulong.MaxValue).Value);
        Assert.AreEqual(new Number(uint.MaxValue).Value, ((Number)uint.MaxValue).Value);
        Assert.AreEqual(new Number(ushort.MaxValue).Value, ((Number)ushort.MaxValue).Value);
        Assert.AreEqual(new Number(byte.MaxValue).Value, ((Number)byte.MaxValue).Value);
    }

    /// <summary>
    /// Tests that the Number converts to a string properly.
    /// </summary>
    [TestMethod]
    public void TestToString()
    {
        Dictionary<string, string> expressions = new Dictionary<string, string>()
        {
            { ".1234", "0.1234" },
            { "0.1234", "0.1234" },
            { "1234.", "1234" },
            { "1234", "1234" },
            { "436.246", "436.246" },
            { "0123", "123" },
            { "1230", "1230" },
            { "489.38400", "489.384" }
        };

        foreach (string expression in expressions.Keys)
            Assert.AreEqual(expressions[expression], new Parser().Parse(expression).ToString());
    }
}
