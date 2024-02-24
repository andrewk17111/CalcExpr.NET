using CalcExpr.Context;

namespace CalcExpr.Expressions;

/// <summary>
/// Initializes a new instance of the the <see cref="Variable"/> class.
/// </summary>
/// <param name="name">The name of the <see cref="Variable"/> for reference.</param>
public class Variable(string name) : IExpression
{
    public readonly string Name = name;

    public IExpression Evaluate()
        => Evaluate(new ExpressionContext());

    public IExpression Evaluate(ExpressionContext variables)
        => variables[Name];
    
    public IExpression StepEvaluate()
        => StepEvaluate(new ExpressionContext());

    public IExpression StepEvaluate(ExpressionContext variables)
        => variables[Name];

    public override bool Equals(object? obj)
        => obj is not null && obj is Variable v && v.Name == Name;

    public override int GetHashCode()
        => Name.GetHashCode();

    public override string ToString()
        => Name;

    public string ToString(string? format)
        => Name;
}
