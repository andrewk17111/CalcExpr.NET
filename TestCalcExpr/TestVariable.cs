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
    /// Tests that the UnaryOperator converts to a string properly.
    /// </summary>
    [TestMethod]
    public void TestToString()
    {
        // TODO Variable ToString.
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
        };

        foreach (UnaryOperator expression in expressions.Keys)
            Assert.AreEqual(expressions[expression], expression.ToString());
    }
}
