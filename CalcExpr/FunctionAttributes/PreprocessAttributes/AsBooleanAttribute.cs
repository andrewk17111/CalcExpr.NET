using CalcExpr.Expressions;

namespace CalcExpr.FunctionAttributes.PreprocessAttributes;

public class AsBooleanAttribute : PreprocessAttribute
{
    public override IExpression Preprocess(IExpression expression)
    {
        if (Undefined.UNDEFINED.Equals(expression))
            return Undefined.UNDEFINED;
        else if (Constant.TRUE.Equals(expression) || Constant.FALSE.Equals(expression))
            return expression;
        else if (new Number(0).Equals(expression))
            return Constant.FALSE;
        else // Any value that is not 0 or undefined should result in true.
            return Constant.TRUE;
    }
}
