namespace CalcExpr.Expressions;

public class Variable : IExpression
{
    public readonly string Name;

    /// <summary>
    /// Initializes a new instance of the the <see cref="Variable"/> class.
    /// </summary>
    /// <param name="name">The name of the <see cref="Variable"/> for reference.</param>
    public Variable(string name)
        => Name = name;

    public IExpression Clone()
        => new Variable(Name);
    
    public IExpression Evaluate()
        => throw new NotImplementedException();

    public IExpression Evaluate(Dictionary<string, IExpression> variables)
        => throw new NotImplementedException();
    
    public IExpression StepEvaluate()
        => throw new NotImplementedException();

    public IExpression StepEvaluate(Dictionary<string, IExpression> variables)
        => throw new NotImplementedException();

    public override bool Equals(object? obj)
        => obj is not null && obj is Variable v && v.Name == Name;

    public override int GetHashCode()
        => Name.GetHashCode();

    public override string ToString()
        => Name;

    public string ToString(string? format)
        => Name;
}
