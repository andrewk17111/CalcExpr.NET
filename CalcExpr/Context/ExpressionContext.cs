using CalcExpr.Expressions;

namespace CalcExpr.Context;

public class ExpressionContext
{
    private Dictionary<string, IExpression> _variables;

    public IExpression this[string variable]
    {
        get => _variables.ContainsKey(variable) ? _variables[variable] : Constant.UNDEFINED;
        set => _variables[variable] = value;
    }

    public ExpressionContext() : this(new Dictionary<string, IExpression>())
    { }

    public ExpressionContext(Dictionary<string, IExpression> variables)
        => _variables = variables ?? new Dictionary<string, IExpression>();

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
