using CalcExpr.Expressions;

namespace CalcExpr.Context;

public class ExpressionContext
{
    private Dictionary<string, IExpression> _variables;
    private Dictionary<string, Function> _functions;

    public IExpression this[string variable]
    {
        get => _variables.ContainsKey(variable) ? _variables[variable] : Constant.UNDEFINED;
        set => _variables[variable] = value;
    }

    public IExpression this[string function, IEnumerable<IExpression> args]
        => (_functions.ContainsKey(function)
            ? (IExpression?)_functions[function].Body.Method.Invoke(function, args.ToArray<object?>())
            : null)
                ?? Constant.UNDEFINED;

    public ExpressionContext(Dictionary<string, IExpression>? variables = null,
        Dictionary<string, Function>? functions = null)
        => _variables = variables ?? new Dictionary<string, IExpression>();

    public bool SetVariable(string name, IExpression expression)
    {
        if (expression is null)
            return _variables.Remove(name);

        _variables[name] = expression;
        return true;
    }

    public bool RemoveVariable(string name)
        => _variables.Remove(name);

    public bool ContainsVariable(string name)
        => _variables.ContainsKey(name);

    public bool SetFunction(string name, Function functions)
        => throw new NotImplementedException();

    public bool RemoveFunction(string name)
        => throw new NotImplementedException();

    public bool ContainsFunction(string name)
        => throw new NotImplementedException();
}
