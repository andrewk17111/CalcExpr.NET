using CalcExpr.Context;

namespace CalcExpr.Expressions.Terminals;

public abstract class Terminal : IExpression
{
    public IExpression Evaluate()
        => this;

    public IExpression Evaluate(ExpressionContext _)
        => this;

    public IExpression StepEvaluate()
        => this;

    public IExpression StepEvaluate(ExpressionContext _)
        => this;

    public override string ToString()
        => ToString(null);

    public abstract string ToString(string? format);
}
