using System.Globalization;

namespace CalcExpr.Expressions;

public interface IExpression
{
    public IExpression StepSimplify();

    public IExpression Simplify();

    public IExpression StepEvaluate();

    public IExpression Evaluate();

    public IExpression Clone();

    public string ToString();

    public string ToString(string? format);
}
