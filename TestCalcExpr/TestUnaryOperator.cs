using CalcExpr.Expressions;

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
        Dictionary<UnaryOperator, double> expressions = new Dictionary<UnaryOperator, double>()
        {
            { new UnaryOperator("+", true, new Number(1)), 1 },
            { new UnaryOperator("-", true, new Number(1)), -1 },
            { new UnaryOperator("~", true, new Number(1)), 0 },
            { new UnaryOperator("¬", true, new Number(1)), 0 },
            { new UnaryOperator("!", true, new Number(5)), 44 },
            { new UnaryOperator("!", false, new Number(5)), 120 },
            { new UnaryOperator("%", false, new Number(1)), 0.01 },
            { new UnaryOperator("!!", false, new Number(5)), 15 },
            { new UnaryOperator("#", false, new Number(5)), 30 },
            // TODO ± prefix.
            // TODO ++ prefix.
            // TODO -- prefix.
            // TODO ++ postfix.
            // TODO -- postfix.
        };

        foreach (UnaryOperator expression in expressions.Keys)
            Assert.AreEqual(expressions[expression], ((Number)expression.Evaluate()).Value);
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
