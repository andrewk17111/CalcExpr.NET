using CalcExpr.Expressions;

namespace CalcExpr.FunctionAttributes.PreprocessAttributes;

public class AsBooleanAttribute : PreprocessAttribute
{
    public override IExpression Preprocess(IExpression expression)
    {
        if (Undefined.UNDEFINED.Equals(expression))
            return Undefined.UNDEFINED;
        else if (Logical.TRUE.Equals(expression) || Logical.FALSE.Equals(expression))
            return expression;
        else if (new Number(0).Equals(expression))
            return Logical.FALSE;
        else // Any value that is not 0 or undefined should result in true.
            return Logical.TRUE;
    }
}
