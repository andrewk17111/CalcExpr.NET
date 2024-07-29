using CalcExpr.Expressions;
using CalcExpr.Expressions.Interfaces;

namespace CalcExpr.FunctionAttributes.PreprocessAttributes;

public class AsBooleanAttribute : PreprocessAttribute
{
    public override IExpression Preprocess(IExpression expression)
    {
        if (expression is Logical)
            return expression;
        else if (expression is ILogicalConvertible convertible)
            return convertible.ToLogical();

        return Undefined.UNDEFINED;
    }
}
