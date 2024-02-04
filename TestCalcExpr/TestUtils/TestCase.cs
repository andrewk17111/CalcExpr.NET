using CalcExpr.Expressions;
using System.Text;

namespace TestCalcExpr.TestUtils;

public readonly struct TestCase
{
    public readonly string ExpressionString;
    public readonly IExpression Parsed;
    public readonly IExpression Evaluated;
    public readonly IExpression[] StepEvaluated;

    public TestCase(string expression_string, IExpression parsed, IExpression? evaluated = null,
        params IExpression[] step_evaluated)
    {
        ExpressionString = expression_string;
        Parsed = parsed;
        Evaluated = evaluated ?? Parsed;
        StepEvaluated = (step_evaluated ?? Array.Empty<IExpression>()).Append(Evaluated).ToArray();
    }
}
