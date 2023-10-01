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
        ("--", true),
        ("++", true),
        ("--", false),
        ("++", false),
        // TODO ± prefix.
    };

    /// <summary>
    /// Tests that the properties of the UnaryOperator are the same as when initialized.
    /// </summary>
    [TestMethod]
    public void TestInit()
    {
        Random random = new Random();

        foreach ((string op, bool is_prefix) in _operators)
        {
            Number num = new Number(random.NextDouble() + random.Next());
            UnaryOperator unop = new UnaryOperator(op, is_prefix, num);

            Assert.AreEqual(op, unop.Identifier);
            Assert.AreEqual(is_prefix, unop.IsPrefix);
            Assert.AreEqual(num, unop.Inside);
        }
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
            { new UnaryOperator("--", true, new Variable("abc")), "--abc" },
            { new UnaryOperator("++", true, new Variable("abc")), "++abc" },
            { new UnaryOperator("--", false, new Variable("abc")), "abc--" },
            { new UnaryOperator("++", false, new Variable("abc")), "abc++" },
            // TODO ± prefix.
        };

        foreach (UnaryOperator expression in expressions.Keys)
            Assert.AreEqual(expressions[expression], expression.ToString());
    }
}
