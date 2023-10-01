using CalcExpr.Expressions;

namespace TestCalcExpr;

[TestClass]
public class TestEvaluation
{
    private const int DIGITS = 6;

    [TestMethod]
    public void TestEvaluate()
    {
        foreach ((string expr_string, IExpression expression, IExpression result) in TestCases.Expressions)
        {
            IExpression evaluated = expression.Evaluate(TestCases.Variables);

            if (result is Number result_num && evaluated is Number evaluated_num)
                Assert.AreEqual(Math.Round(result_num.Value, DIGITS), Math.Round(evaluated_num.Value, DIGITS));
            else
                Assert.AreEqual(result, evaluated);
        }
    }
}
