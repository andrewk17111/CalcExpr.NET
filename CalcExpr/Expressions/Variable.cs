namespace CalcExpr.Expressions;

public class Variable : IExpression
{
    public readonly string Name;

    /// <summary>
    /// Initializes a new instance of the the <see cref="Variable"/> class.
    /// </summary>
    /// <param name="name">The name of the <see cref="Variable"/> for reference.</param>
    public Variable(string name)
        => throw new NotImplementedException();

    public IExpression Clone()
        => throw new NotImplementedException();
    
    public IExpression Evaluate()
        => throw new NotImplementedException();

    public IExpression Evaluate(Dictionary<string, IExpression> variables)
        => throw new NotImplementedException();
    
    public IExpression StepEvaluate()
        => throw new NotImplementedException();

    public IExpression StepEvaluate(Dictionary<string, IExpression> variables)
        => throw new NotImplementedException();

    public override bool Equals(object? obj)
        => throw new NotImplementedException();

    public override int GetHashCode()
        => throw new NotImplementedException();

    public override string ToString()
        => throw new NotImplementedException();

    public string ToString(string? format)
        => throw new NotImplementedException();
}
