using CalcExpr.Expressions;

namespace CalcExpr.Context;

public class ExpressionContext(Dictionary<string, IExpression> variables)
{
    private readonly Dictionary<string, IExpression> _variables = variables ?? [];

    public IExpression this[string variable]
    {
        get => _variables.TryGetValue(variable, out IExpression? value) ? value : Constant.UNDEFINED;
        set => _variables[variable] = value;
    }

    public ExpressionContext() : this([])
    { }

    public bool SetVariable(string variable, IExpression expression)
    {
        if (expression is null)
            return _variables.Remove(variable);

        _variables[variable] = expression;
        return true;
    }

    public bool RemoveVariable(string variable)
        => _variables.Remove(variable);

    public bool ContainsVariable(string variable)
        => _variables.ContainsKey(variable);
}
