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

            IExpression result = test_case.Parsed.Evaluate(context);

            UtilFunctions.AreEqual(test_case.Evaluated, result, DIGITS);
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

                UtilFunctions.AreEqual(test_case.StepEvaluated[i], evaluated, DIGITS);
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

                    UtilFunctions.AreEqual(evaluated, result, DIGITS);
                }
            }
        }
    }
}
