using CalcExpr.Context;
using CalcExpr.Expressions;
using System.Reflection;
using TestCalcExpr.TestData;
using TestCalcExpr.TestUtils;

namespace TestCalcExpr;

[TestClass]
public class TestEvaluation
{
    private const int DIGITS = 6;

    [TestMethod]
    public void TestEvaluate()
    {
        EvaluateTestCases(TestCases.Expressions);
    }

    [TestMethod]
    public void TestEvaluateCollections()
    {
        EvaluateTestCases(TestCases.Collections);
    }

    private static void EvaluateTestCases(IEnumerable<TestCase> test_cases)
    {
        foreach (TestCase test_case in test_cases)
        {
            ExpressionContext context = new ExpressionContext(TestCases.ContextVariables);

            foreach (string func in TestCases.ContextFunctions.Keys)
                context[func] = TestCases.ContextFunctions[func];

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
        StepEvaluateTestCases(TestCases.Expressions);
    }

    [TestMethod]
    public void TestStepEvaluateCollections()
    {
        EvaluateTestCases(TestCases.Collections);
    }

    private static void StepEvaluateTestCases(IEnumerable<TestCase> test_cases)
    {
        foreach (TestCase test_case in test_cases)
        {
            ExpressionContext context = new ExpressionContext(TestCases.ContextVariables);

            foreach (string func in TestCases.ContextFunctions.Keys)
                context[func] = TestCases.ContextFunctions[func];

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

    [TestMethod]
    public void TestEvaluateFunctions()
    {
        IEnumerable<FunctionTestCase[]?> test_case_groups = typeof(TestCases)
            .GetFields(BindingFlags.Public | BindingFlags.Static)
            .Where(field => field.FieldType == typeof(FunctionTestCase[]))
            .Select(field => (FunctionTestCase[]?)field.GetValue(null));

        foreach (FunctionTestCase[]? test_cases in test_case_groups)
        {
            if (test_cases is null)
                continue;

            foreach (FunctionTestCase test_case in test_cases)
            {
                int i = 0;

                foreach ((IExpression[] args, IExpression evaluated) in test_case.Evaluated)
                {
                    int j = i++ % test_case.FunctionAliases.Length;

                    IExpression result = new FunctionCall(test_case.FunctionAliases[j], args)
                        .Evaluate(test_case.Context ?? new ExpressionContext());

                    if (evaluated is Number eval_num && result is Number result_num)
                        Assert.AreEqual(Math.Round(eval_num.Value, DIGITS), Math.Round(result_num.Value, DIGITS));
                    else
                        Assert.AreEqual(evaluated, result);
                }
            }
        }
    }
}
