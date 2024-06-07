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

        return Constant.UNDEFINED;
    }

    public bool? ConvertFromExpression(IExpression expression)
    {
        IExpression result = new AsBooleanAttribute().Preprocess(expression);

        if (Constant.UNDEFINED.Equals(result))
            return null;

        return Constant.TRUE.Equals(result);
    }
}
