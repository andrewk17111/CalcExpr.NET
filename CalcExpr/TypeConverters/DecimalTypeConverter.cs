using CalcExpr.Expressions;

namespace CalcExpr.TypeConverters;

public class DecimalTypeConverter : ITypeConverter<decimal?>
{
    public IExpression ConvertToExpression(decimal? value)
    {
        try
        {
            if (value.HasValue)
                return (Number)Convert.ToDouble(value.Value);
        }
        catch
        {
        }

        return Constant.UNDEFINED;
    }

    public decimal? ConvertFromExpression(IExpression expression)
    {
        try
        {
            if (expression is Number num)
                return Convert.ToDecimal(num.Value);
        }
        catch
        {
        }

        return null;
    }
}
