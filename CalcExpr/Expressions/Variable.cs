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
        => Evaluate(null);

    public IExpression Evaluate(Dictionary<string, IExpression>? variables)
        => variables is not null && variables.ContainsKey(Name)
            ? variables[Name].Evaluate(variables)
            : throw new Exception(); // TODO.
    
    public IExpression StepEvaluate()
        => StepEvaluate(null);

    public IExpression StepEvaluate(Dictionary<string, IExpression>? variables)
        => variables is not null && variables.ContainsKey(Name)
            ? variables[Name].Evaluate(variables)
            : throw new Exception(); // TODO.

    public override bool Equals(object? obj)
        => obj is not null && obj is Variable v && v.Name == Name;

    public override int GetHashCode()
        => Name.GetHashCode();

    public override string ToString()
        => Name;

    public string ToString(string? format)
        => Name;
}
