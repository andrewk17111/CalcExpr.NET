using CalcExpr.Expressions;
using CalcExpr.Parsing;

namespace TestCalcExpr;

[TestClass]
public class TestParser
{
    /// <summary>
    /// Tests that the Parser is able to properly parse strings into IExpressions.
    /// </summary>
    [TestMethod]
    public void TestParse()
    {
        Dictionary<string, IExpression> expressions = new Dictionary<string, IExpression>()
        {
            { ".1", new Number(0.1) },
            { "0.1", new Number(0.1) },
            { "1.", new Number(1) },
            { "1", new Number(1) },
            { "+1", new UnaryOperator("+", true, new Number(1)) },
            { "-1", new UnaryOperator("-", true, new Number(1)) },
            { "!1", new UnaryOperator("!", true, new Number(1)) },
            { "~2", new UnaryOperator("~", true, new Number(2)) },
            { "¬0", new UnaryOperator("¬", true, new Number(0)) },
            { "5!", new UnaryOperator("!", false, new Number(5)) },
            { "1%", new UnaryOperator("%", false, new Number(1)) },
            { "~!1", new UnaryOperator("~", true, new UnaryOperator("!", true, new Number(1))) },
            { "2!%", new UnaryOperator("%", false, new UnaryOperator("!", false, new Number(2))) },
            { "-5%", new UnaryOperator("-", true, new UnaryOperator("%", false, new Number(5))) }
        };

        foreach (string expression in expressions.Keys)
            Assert.AreEqual(expressions[expression], new Parser().Parse(expression));
    }
}
