using CalcExpr.Context;
using CalcExpr.Expressions;

namespace TestCalcExpr;

[TestClass]
public class TestEvaluation
{
    private const int DIGITS = 6;

    [TestMethod]
    public void TestEvaluate()
    {
        foreach ((_, IExpression expression, IExpression result) in TestCases.Expressions)
        {
            ExpressionContext context = new ExpressionContext(TestCases.Variables, TestCases.Functions);
            IExpression evaluated = expression.Evaluate(context);

            if (result is Number result_num && evaluated is Number evaluated_num)
                Assert.AreEqual(Math.Round(result_num.Value, DIGITS), Math.Round(evaluated_num.Value, DIGITS));
            else
                Assert.AreEqual(result, evaluated);
        }
    }
}
