using CalcExpr.Expressions;

namespace TestCalcExpr;

[TestClass]
public class TestConstant
{
    static readonly string[] CONSTANTS =
    {
        "∞",
        "inf",
        "infinity",
        "π",
        "pi",
        "τ",
        "tau",
        "e",
        "true",
        "false",
    };

    /// <summary>
    /// Tests that the value of the Constant is the same as the value passed into the Constant's constructor.
    /// </summary>
    [TestMethod]
    public void TestInit()
    {
        foreach (string constant in CONSTANTS)
            Assert.AreEqual(constant, new Constant(constant).Identifier);
    }

    /// <summary>
    /// Tests that the Constant gets evaluated into the proper value.
    /// </summary>
    [TestMethod]
    public void TestEvaluate()
    {
        Dictionary<Constant, IExpression> constants = new Dictionary<Constant, IExpression>()
        {
            { new Constant("∞"), new Constant("∞") },
            { new Constant("inf"), new Constant("inf") },
            { new Constant("infinity"), new Constant("infinity") },
            { new Constant("π"), new Number(Math.PI) },
            { new Constant("pi"), new Number(Math.PI) },
            { new Constant("τ"), new Number(Math.Tau) },
            { new Constant("tau"), new Number(Math.Tau) },
            { new Constant("e"), new Number(Math.E) },
            { new Constant("true"), new Number(1) },
            { new Constant("false"), new Number(0) },
        };

        foreach (Constant constant in constants.Keys)
            Assert.AreEqual(constants[constant], constant.Evaluate());
    }

    /// <summary>
    /// Tests that the Constant converts to a string properly.
    /// </summary>
    [TestMethod]
    public void TestToString()
    {
        foreach (string constant in CONSTANTS)
            Assert.AreEqual(constant, new Constant(constant).ToString());
    }
}
