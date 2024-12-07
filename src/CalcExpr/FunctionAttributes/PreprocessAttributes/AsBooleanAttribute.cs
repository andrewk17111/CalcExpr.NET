using CalcExpr.Expressions;
using CalcExpr.Expressions.Interfaces;
using CalcExpr.Expressions.Terminals;

namespace CalcExpr.FunctionAttributes.PreprocessAttributes;

public class AsBooleanAttribute : PreprocessAttribute
{
    public override IExpression Preprocess(IExpression expression)
    {
        return (IExpression?)ILogicalConvertible.ConvertToLogical(expression) ?? Undefined.UNDEFINED;
    }
}
