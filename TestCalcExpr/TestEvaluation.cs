using CalcExpr.Context;
using CalcExpr.Expressions;
using CalcExpr.Expressions.Collections;
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

            UtilFunctions.AreEqual(test_case.Evaluated, result, DIGITS, $"Test case: '{test_case.ExpressionString}'.");
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

                UtilFunctions.AreEqual(test_case.StepEvaluated[i], evaluated, DIGITS,
                    $"Test case: '{test_case.ExpressionString}'.");
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
                    string error_message = "Test case: " +
                        $"'{test_case.FunctionAliases[j]}({String.Join(", ", args.Select(x => x.ToString()))})'.";

                    UtilFunctions.AreEqual(evaluated, result, DIGITS, error_message);
                }
            }
        }

        // The 'random' function is tested separately because it is non-deterministic.
        Random random = new Random();

        for (int i = 0; i < 20; i++)
        {
            Number min = (Number)(random.NextDouble() * Double.MinValue);
            Number max = (Number)(random.NextDouble() * Double.MaxValue);

            IExpression results = new FunctionCall("random", [(Number)i, min, max, Constant.TRUE])
                .Evaluate(new ExpressionContext());

            if (results is IEnumerableExpression true_enum_expr)
            {
                Assert.AreEqual(i, true_enum_expr.Count());
                Assert.IsTrue(true_enum_expr.All(x => x is Number n && n.Value >= min.Value && n.Value <= max.Value));
            }

            results = new FunctionCall("random", [(Number)i, min, max, Constant.FALSE])
                .Evaluate(new ExpressionContext());

            if (results is IEnumerableExpression false_enum_expr)
            {
                Assert.AreEqual(i, false_enum_expr.Count());
                Assert.IsTrue(false_enum_expr.All(x => x is Number n && n.Value >= min.Value && n.Value <= max.Value));
            }
        }
    }
}
