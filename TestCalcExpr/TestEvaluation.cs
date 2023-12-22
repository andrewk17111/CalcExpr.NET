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
        foreach (TestCase test_case in TestCases.Expressions)
        {
            Console.WriteLine(test_case.ExpressionString);
            ExpressionContext context = new ExpressionContext(TestCases.Variables);

            foreach (string func in TestCases.Functions.Keys)
                context[func] = TestCases.Functions[func];

            IExpression evaluated = test_case.Parsed.Evaluate(context);

            if (test_case.Evaluated is Number result_num && evaluated is Number evaluated_num)
                Assert.AreEqual(Math.Round(result_num.Value, DIGITS), Math.Round(evaluated_num.Value, DIGITS));
            else
                Assert.AreEqual(test_case.Evaluated, evaluated);
        }
    }

    [TestMethod]
    public void TestStepEvaluate()
    {
        foreach (TestCase test_case in TestCases.Expressions)
        {
            ExpressionContext context = new ExpressionContext(TestCases.Variables);

            foreach (string func in TestCases.Functions.Keys)
                context[func] = TestCases.Functions[func];

            IExpression start = test_case.Parsed;

            for (int i = 0; i < test_case.StepEvaluated.Length; i++)
            {
                IExpression evaluated = start.StepEvaluate(context);

                if (test_case.StepEvaluated[i] is Number result_num && evaluated is Number evaluated_num)
                    Assert.AreEqual(Math.Round(result_num.Value, DIGITS), Math.Round(evaluated_num.Value, DIGITS));
                else
                    Assert.AreEqual(test_case.StepEvaluated[i], evaluated);

                start = evaluated;
            }
        }
    }
}
