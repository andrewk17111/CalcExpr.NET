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
        ("!", true),
        ("~", true),
        ("¬", true),
        ("!", false),
        ("%", false),
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
    /// Tests that the UnaryOperator parsed from the input string properly assignes the corresponding symbol, whether
    /// it's a prefix or suffix operator, and the expression the operator applies to.
    /// </summary>
    [TestMethod]
    public void TestParse()
    {
        Random random = new Random();

        foreach ((string op, bool is_prefix) in _operators)
        {
            string expression = random.Next().ToString();
            int count = random.Next(10);

            expression = is_prefix ? op + expression : expression + op;

            for (int i = 0; i < count; i++)
            {
                (string new_op, bool new_is_prefix) = _operators[random.Next(_operators.Length)];

                expression = new_is_prefix ? new_op + expression : expression + new_op;
            }

            Assert.AreEqual(expression, new Parser().Parse(expression).ToString());
        }
    }

    /// <summary>
    /// Tests that if the operand has a value (I.E. a Number), the expression gets evaluated, but doesn't get touched if
    /// it's a placeholder (I.E. a Variable).
    /// </summary>
    [TestMethod]
    public void TestSimplify()
    {
        Dictionary<string, double> expressions = new Dictionary<string, double>()
        {
            { "+1", 1 },
            { "-1", -1 },
            { "!1", 0 },
            { "~1", 0 },
            { "¬1", 0 },
            { "5!", 120 },
            { "1%", 0.01 },
            // TODO ± prefix.
            // TODO ++ prefix.
            // TODO -- prefix.
            // TODO ++ suffix.
            // TODO -- suffix.
        };

        foreach (string expression in expressions.Keys)
            Assert.AreEqual(expressions[expression], ((Number)new Parser().Parse(expression).Simplify()).Value);
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
            { "!1", 0 },
            { "~1", 0 },
            { "¬1", 0 },
            { "5!", 120 },
            { "1%", 0.01 },
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
            "!1",
            "~1",
            "¬1",
            "5!",
            "1%"
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
