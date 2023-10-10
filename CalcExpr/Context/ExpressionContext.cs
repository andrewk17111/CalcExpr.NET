using CalcExpr.Expressions;

namespace CalcExpr.Context;

public class ExpressionContext
{
    public IExpression this[string variable]
    {
        get => throw new NotImplementedException();
        set => throw new NotImplementedException();
    }

    public ExpressionContext()
        => throw new NotImplementedException();

    public ExpressionContext(Dictionary<string, IExpression> variables)
        => throw new NotImplementedException();

    public bool AddVariable(string variable, IExpression expression)
        => throw new NotImplementedException();

    public bool RemoveVariable(string variable)
        => throw new NotImplementedException();

    public bool ContainsVariable(string variable)
        => throw new NotImplementedException();
}
