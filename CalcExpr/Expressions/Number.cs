namespace CalcExpr.Expressions;

public class Number : IExpression
{
    public double Value { get; private set; }

    public Number(double value)
        => Value = value;

    public IExpression Evaluate()
        => Clone();

    public IExpression Simplify()
        => Clone();

    public IExpression StepEvaluate()
        => Clone();

    public IExpression StepSimplify()
        => Clone();

    public IExpression Clone()
        => new Number(Value);
}
