using CalcExpr.Expressions;
using CalcExpr.Parsing;

namespace TestCalcExpr;

[TestClass]
public class TestUnaryOperator
{
    private (string, bool)[] _operators =
    {
        ("+", true),
        ("-", true),
        ("~", true),
        ("¬", true),
        ("!", true),
        ("!", false),
        ("%", false),
        ("!!", false),
        ("#", false),
        // TODO ± prefix.
        // TODO ++ prefix.
        // TODO -- prefix.
        // TODO ++ suffix.
        // TODO -- suffix.
    };

    /// <summary>
    /// Tests that the properties of the UnaryOperator are the same as when initialized.
    /// </summary>
    [TestMethod]
    public void TestInit()
    {
        Random random = new Random();
        Number num = new Number(1);

        foreach ((string op, bool is_prefix) in _operators)
        {
            UnaryOperator unop = new UnaryOperator(op, is_prefix, num);

            Assert.AreEqual(op, unop.Identifier);
            Assert.AreEqual(is_prefix, unop.IsPrefix);
            Assert.AreEqual(num, unop.Inside);
        }
    }

    /// <summary>
    /// Tests that the operand gets evaluated into a simpler expression with the minimum levels to the expression tree.
    /// </summary>
    [TestMethod]
    public void TestEvaluate()
    {
        Dictionary<string, double> expressions = new Dictionary<string, double>()
        {
            { "+1", 1 },
            { "-1", -1 },
            { "~1", 0 },
            { "¬1", 0 },
            { "!5", 44 },
            { "5!", 120 },
            { "1%", 0.01 },
            { "5!!", 15 },
            { "5#", 30 },
            // TODO ± prefix.
            // TODO ++ prefix.
            // TODO -- prefix.
            // TODO ++ suffix.
            // TODO -- suffix.
        };

        foreach (string expression in expressions.Keys)
            Assert.AreEqual(expressions[expression], ((Number)new Parser().Parse(expression).Evaluate()).Value);
    }

    /// <summary>
    /// Tests that the UnaryOperator converts to a string properly.
    /// </summary>
    [TestMethod]
    public void TestToString()
    {
        string[] expressions =
        {
            "+1",
            "-1",
            "~1",
            "¬1",
            "!5",
            "5!",
            "1%",
            "5!!",
            "5#",
            // TODO ± prefix.
            // TODO ++ prefix.
            // TODO -- prefix.
            // TODO ++ suffix.
            // TODO -- suffix.
        };

        foreach (string expression in expressions)
            Assert.AreEqual(expression, new Parser().Parse(expression).ToString());
    }
}
