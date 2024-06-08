using DiceEngine.Expressions;
using DiceEngine.FunctionAttributes.PreprocessAttributes;

namespace DiceEngine.TypeConverters;

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

    public bool? ConvertFromExpression(IExpression? expression)
    {
        if (expression is null)
            return null;

        IExpression result = new AsBooleanAttribute().Preprocess(expression);

        if (Constant.UNDEFINED.Equals(result))
            return null;

        return Constant.TRUE.Equals(result);
    }
}
