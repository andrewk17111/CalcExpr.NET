using CalcExpr.Expressions;
using CalcExpr.FunctionAttributes.PreprocessAttributes;

namespace CalcExpr.TypeConverters;

public class BooleanTypeConverter : ITypeConverter<bool?>
{
    public IExpression ConvertToExpression(bool? value)
    {
        if (value.HasValue)
        {
            return value.Value ? Constant.TRUE : Constant.FALSE;
        }

        return Undefined.UNDEFINED;
    }

    public bool? ConvertFromExpression(IExpression? expression)
    {
        if (expression is null)
            return null;

        IExpression result = new AsBooleanAttribute().Preprocess(expression);

        if (Undefined.UNDEFINED.Equals(result))
            return null;

        return Constant.TRUE.Equals(result);
    }
}
