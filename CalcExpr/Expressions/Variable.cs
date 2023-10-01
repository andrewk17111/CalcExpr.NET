using CalcExpr.Exceptions;

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
        => Evaluate(new Dictionary<string, IExpression>());

    public IExpression Evaluate(Dictionary<string, IExpression> variables)
        => variables.ContainsKey(Name)
            ? variables[Name]
            : Constant.UNDEFINED;
    
    public IExpression StepEvaluate()
        => StepEvaluate(new Dictionary<string, IExpression>());

    public IExpression StepEvaluate(Dictionary<string, IExpression> variables)
        => variables.ContainsKey(Name)
            ? variables[Name]
            : Constant.UNDEFINED;

    public override bool Equals(object? obj)
        => obj is not null && obj is Variable v && v.Name == Name;

    public override int GetHashCode()
        => Name.GetHashCode();

    public override string ToString()
        => Name;

    public string ToString(string? format)
        => Name;
}
