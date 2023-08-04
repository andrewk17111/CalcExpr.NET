using System.Globalization;

namespace CalcExpr.Expressions;

public interface IExpression
{
    public IExpression Evaluate();

    public IExpression StepEvaluate();

    public IExpression Clone();

    public string ToString();

    public string ToString(string? format);
}
