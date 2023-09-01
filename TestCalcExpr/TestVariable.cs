using CalcExpr.Expressions;

namespace TestCalcExpr;

[TestClass]
public class TestVariable
{
    static readonly string[] VALID_VARIABLES =
    {
        "AaBbCcDdEeFfGgHhIiJjKkLlMmNnOoPpQqRrSsTtUuVvWwXxYyZz",
        "ΑαΒβΓγΔδΕεΖζΗηΘθΙιΚκΛλΜμΝνΞξΟοΠπΡρΣσςΤτΥυΦφΧχΨψΩω",
        "abc_123",
        "αβγ_123",
        "abcd_αβγ_xyz",
    };

    /// <summary>
    /// Tests that the name of the Variable is unchanged when initialized.
    /// </summary>
    [TestMethod]
    public void TestInit()
    {
        foreach (string name in VALID_VARIABLES)
            Assert.AreEqual(name, new Variable(name).Name);
    }

    /// <summary>
    /// Tests that the variable gets evaluated to the proper value.
    /// </summary>
    [TestMethod]
    public void TestEvaluate()
    {
        Dictionary<string, IExpression> variables = new Dictionary<string, IExpression>()
        {
            { "AaBbCcDdEeFfGgHhIiJjKkLlMmNnOoPpQqRrSsTtUuVvWwXxYyZz", new Number(1) },
            { "ΑαΒβΓγΔδΕεΖζΗηΘθΙιΚκΛλΜμΝνΞξΟοΠπΡρΣσςΤτΥυΦφΧχΨψΩω", new Number(2) },
            { "abc_123", new Number(3) },
            { "αβγ_123", new Number(4) },
            { "abcd_αβγ_xyz", new Number(5) },
        };

        foreach (string name in VALID_VARIABLES)
            Assert.AreEqual(variables[name], ((Number)new Variable(name).Evaluate(variables)).Value);
    }

    /// <summary>
    /// Tests that the UnaryOperator converts to a string properly.
    /// </summary>
    [TestMethod]
    public void TestToString()
    {
        Dictionary<UnaryOperator, string> expressions = new Dictionary<UnaryOperator, string>()
        {
            { new UnaryOperator("+", true, new Number(1)), "+1" },
            { new UnaryOperator("-", true, new Number(1)), "-1" },
            { new UnaryOperator("~", true, new Number(1)), "~1" },
            { new UnaryOperator("¬", true, new Number(1)), "¬1" },
            { new UnaryOperator("!", true, new Number(5)), "!5" },
            { new UnaryOperator("!", false, new Number(5)), "5!" },
            { new UnaryOperator("%", false, new Number(1)), "1%" },
            { new UnaryOperator("!!", false, new Number(5)), "5!!" },
            { new UnaryOperator("#", false, new Number(5)), "5#" },
            // TODO ± prefix.
            // TODO ++ prefix.
            // TODO -- prefix.
            // TODO ++ suffix.
            // TODO -- suffix.
        };

        foreach (UnaryOperator expression in expressions.Keys)
            Assert.AreEqual(expressions[expression], expression.ToString());
    }
}
